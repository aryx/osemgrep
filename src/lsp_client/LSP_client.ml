(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Use semgrep as an LSP *client* connecting to a language server
 * (ocamllsp, clangd, etc.) to get type information for identifiers
 * during pattern matching. This allows semgrep to leverage the language
 * server's type knowledge to improve matching accuracy.
 *
 * Originally written in 2020 (OCaml only), restored and modernized in
 * 2026 with multi-language support.
 *
 * Supported language servers:
 *  - OCaml: ocamllsp (run from the project dir so it finds .cmt files)
 *  - C: clangd (uses compile_commands.json or heuristics)
 *  - C++: clangd (same as C; uses compile_commands.json or heuristics)
 *  - Go: gopls (needs go.mod; project root auto-detected)
 *
 * To use: run osemgrep from the project directory you want to analyze.
 *)

open Common
open Lsp.Types
module G = AST_generic

(*****************************************************************************)
(* Synchronous I/O module for Lsp.Io functor *)
(*****************************************************************************)

(* Identity monad — we do everything synchronously *)
module Sync = struct
  type 'a t = 'a

  let return x = x
  let raise exn = Stdlib.raise exn

  module O = struct
    let ( let+ ) x f = f x
    let ( let* ) x f = f x
  end
end

(* Channel implementation over stdlib in_channel/out_channel *)
module Chan = struct
  type input = in_channel
  type output = out_channel

  let read_line ic =
    try Some (Stdlib.input_line ic)
    with End_of_file -> None

  let read_exactly ic n =
    try
      let buf = Bytes.create n in
      Stdlib.really_input ic buf 0 n;
      Some (Bytes.unsafe_to_string buf)
    with End_of_file -> None

  let write oc strings =
    List.iter (Stdlib.output_string oc) strings;
    Stdlib.flush oc
end

module Io = Lsp.Io.Make (Sync) (Chan)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)
type conn = {
  ic: in_channel;
  oc: out_channel;
}

type env = {
  conn: conn option;
  last_uri: string;
  lang: Lang.t;
}

let global = ref {
  conn = None;
  last_uri = "";
  lang = Lang.Ocaml;
}

let debug = ref false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO: make this configurable via CLI flag or env var *)
let server lang =
  match lang with
  | Lang.Ocaml ->
      (* Look for ocamllsp in the current opam switch *)
      let opam_bin =
        try String.trim (UCmd.cmd_to_list "opam var bin" |> List.hd)
        with _exn -> "/usr/bin"
      in
      Filename.concat opam_bin "ocamllsp"
  | Lang.C | Lang.Cpp ->
      (* clangd is typically in PATH; uses compile_commands.json for
       * project-specific flags. Handles both C and C++. *)
      "clangd"
  | Lang.Go ->
      "gopls"
  | lang ->
      failwith (spf "LSP_client: unsupported language: %s" (Lang.show lang))

(* Walk up from [dir] looking for a file matching [marker].
 * Returns the first directory containing it, or None. *)
let rec find_marker_upward marker dir =
  let candidate = Filename.concat dir marker in
  if Sys.file_exists candidate then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None (* reached filesystem root *)
    else find_marker_upward marker parent

(* Given a set of scanning roots, find the project root directory
 * for the language server.  Each language has its own marker file:
 *  - Go: go.mod
 *  - OCaml: dune-project (or _build)
 *  - C: compile_commands.json (or Makefile)
 *
 * We start from the first scanning root and walk upward.
 * Falls back to CWD if no marker is found. *)
let find_project_root lang (roots : string list) =
  let marker =
    match lang with
    | Lang.Go -> "go.mod"
    | Lang.Ocaml -> "dune-project"
    | Lang.C | Lang.Cpp -> "compile_commands.json"
    | _ -> ""
  in
  if marker = "" then Sys.getcwd ()
  else
    let start_dir =
      let dir =
        match roots with
        | root :: _ ->
            if Sys.is_directory root then root
            else Filename.dirname root
        | [] -> Sys.getcwd ()
      in
      (* Make absolute so the LSP rootUri is a proper file:// URI *)
      if Filename.is_relative dir
      then Filename.concat (Sys.getcwd ()) dir
      else dir
    in
    let root =
      match find_marker_upward marker start_dir with
      | Some dir -> dir
      | None -> start_dir
    in
    Logs.info (fun m -> m "LSP_client: found project root at %s (marker=%s)" root marker);
    root

(*****************************************************************************)
(* LSP JSON-RPC helpers *)
(*****************************************************************************)
let counter = ref 0

let send_request (type a) (req : a Lsp.Client_request.t) conn =
  incr counter;
  let id = `Int !counter in
  let json_rpc = Lsp.Client_request.to_jsonrpc_request req ~id in
  let packet = Jsonrpc.Packet.Request json_rpc in
  Io.write conn.oc packet;
  id

let send_notif notif conn =
  let json_rpc = Lsp.Client_notification.to_jsonrpc notif in
  let packet = Jsonrpc.Packet.Notification json_rpc in
  Io.write conn.oc packet

let rec read_response : type a. Jsonrpc.Id.t * a Lsp.Client_request.t -> conn -> a =
  fun (id, req) conn ->
  let res = Io.read conn.ic in
  match res with
  | None -> failwith "LSP_client: no answer from server"
  | Some packet ->
     (match packet with
     | Jsonrpc.Packet.Notification
       { Jsonrpc.Notification.method_ = "textDocument/publishDiagnostics"; _ } ->
            (* skip diagnostics notifications, keep reading *)
            read_response (id, req) conn
     | Jsonrpc.Packet.Notification _ ->
            (* skip other notifications *)
            read_response (id, req) conn
     | Jsonrpc.Packet.Response { Jsonrpc.Response.id = id2; result } ->
         if not (Jsonrpc.Id.equal id2 id)
         then failwith (spf "LSP_client: id mismatch: got %s, expected %s"
                          (Dumper.dump id2) (Dumper.dump id));
         (match result with
         | Ok json ->
             Lsp.Client_request.response_of_json req json
         | Error err ->
             let json = Jsonrpc.Response.Error.yojson_of_t err in
             let s = Yojson.Safe.pretty_to_string json in
             failwith (spf "LSP_client: server error: %s" s)
         )
     | Jsonrpc.Packet.Request _ ->
            (* server-initiated request; skip for now *)
            read_response (id, req) conn
     | Jsonrpc.Packet.Batch_response _
     | Jsonrpc.Packet.Batch_call _ ->
            failwith "LSP_client: unexpected batch packet"
     )

(*****************************************************************************)
(* Language-specific helpers *)
(*****************************************************************************)

(* Return the LSP languageId string for didOpen notifications *)
let language_id lang =
  match lang with
  | Lang.Ocaml -> "ocaml"
  | Lang.C -> "c"
  | Lang.Cpp -> "cpp"
  | Lang.Go -> "go"
  | lang -> failwith (spf "LSP_client: unsupported language: %s" (Lang.show lang))

(* Language dispatch: clean hover string *)
let clean_hover_string lang s =
  match lang with
  | Lang.Ocaml -> LSP_ocaml.clean_hover s
  | Lang.C | Lang.Cpp -> LSP_c.clean_hover s
  | Lang.Go -> LSP_go.clean_hover s
  | lang ->
      failwith (spf "LSP_client: hover cleanup not supported for %s"
                  (Lang.show lang))

(* Language dispatch: parse type string *)
let parse_type_string lang s =
  match lang with
  | Lang.Ocaml -> LSP_ocaml.parse_type s
  | Lang.C | Lang.Cpp -> LSP_c.parse_type s
  | Lang.Go -> LSP_go.parse_type s
  | lang ->
      failwith (spf "LSP_client: type parsing not supported for %s"
                  (Lang.show lang))

(*****************************************************************************)
(* Hover query *)
(*****************************************************************************)

let type_at_tok tk (uri : DocumentUri.t) conn =
  let lang = !global.lang in
  let line = Tok.line_of_tok tk in
  let col = Tok.col_of_tok tk in
  if !debug then UCommon.pr2_gen (line, col);
  (* LSP uses 0-based lines and columns *)
  let line = line - 1 in

  let req = Lsp.Client_request.TextDocumentHover
         (HoverParams.create
           ~textDocument:(TextDocumentIdentifier.create ~uri)
           ~position:(Position.create ~line ~character:col)
           ()) in
  let id = send_request req conn in
  let res = read_response (id, req) conn in
  if !debug then UCommon.pr2_gen res;
  match res with
  | None ->
      if !debug then UCommon.pr2 (spf "NO TYPE INFO for %s"
                                    (Tok.content_of_tok tk));
      None
  | Some { Hover.contents = x; _ } ->
      (match x with
      | `MarkupContent { MarkupContent.value = s; kind = _ } ->
            if !debug then UCommon.pr2_gen x;
            if !debug then UCommon.pr2 (spf "RAW hover: [%s]" s);
            let s = clean_hover_string lang s in
            if !debug then UCommon.pr2 (spf "CLEANED hover: [%s]" s);
            let ty =
              try parse_type_string lang s
              with exn ->
                  if !debug then
                    UCommon.pr2_gen ("Exn parse_type_string", s, exn);
                  raise exn
            in
            if !debug then UCommon.pr2_gen ty;
            Some ty
      | _ ->
            failwith "LSP_client: hover response not a MarkupContent"
      )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let connect_server ~root lang =
  (* the PWD of the server process is used to look for the .cmt so
   * run this program from the project you want to analyze *)
  let cmd = server lang in
  let ic, oc = Unix.open_process cmd in
  let conn = { ic; oc } in

  if !debug then UCommon.pr2 (spf "LSP_client: rootUri=%s" root);
  let root_uri = DocumentUri.of_path root in
  let capabilities = ClientCapabilities.create () in
  let params = InitializeParams.create
      ~capabilities
      ~rootUri:root_uri
      ~workspaceFolders:(Some [
        WorkspaceFolder.create ~uri:root_uri ~name:"root"
      ])
      ~trace:TraceValues.Verbose
      () in
  let req = Lsp.Client_request.Initialize params in
  let id = send_request req conn in
  let res = read_response (id, req) conn in
  if !debug then UCommon.pr2_gen res;
  let notif = Lsp.Client_notification.Initialized in
  send_notif notif conn;
  conn

(* Ensure the file is opened in the LSP server, then call [f] with
 * the connection and URI. Handles didOpen/didClose as needed. *)
let rec with_file_open tok f =
  let file = Tok.file_of_tok tok in
  (* Make absolute so the file:// URI matches the workspace root *)
  let path = Fpath.to_string file in
  let abs_path =
    if Filename.is_relative path
    then Filename.concat (Sys.getcwd ()) path
    else path
  in
  let uri = DocumentUri.of_path abs_path in
  let uri_s = DocumentUri.to_string uri in
  match !global with
  | { conn = Some conn; last_uri; _ } when last_uri = uri_s ->
      (try f tok uri conn
      with exn ->
        if !debug then UCommon.pr2 (spf "LSP_client: exception in hover: %s"
                                      (Printexc.to_string exn));
        None
      )
  | { conn = Some conn; last_uri; _ } when last_uri <> uri_s ->
      if last_uri <> ""
      then begin
        let prev_uri = DocumentUri.of_path last_uri in
        let notif = Lsp.Client_notification.TextDocumentDidClose
            (DidCloseTextDocumentParams.create
              ~textDocument:(TextDocumentIdentifier.create ~uri:prev_uri)
             )
        in
        send_notif notif conn;
       end;

      let notif = Lsp.Client_notification.TextDocumentDidOpen
          (DidOpenTextDocumentParams.create
            ~textDocument: (TextDocumentItem.create
              ~uri
              ~text: (UFile.read_file file)
              ~version:1
              ~languageId:(language_id !global.lang)
          )
        )
      in
      send_notif notif conn;
      global := { !global with last_uri = uri_s };
      (* try again *)
      with_file_open tok f

  | _ ->
     None

let get_type (id : G.ident) =
  with_file_open (snd id) type_at_tok

(* Like get_type but works on any expression, not just identifiers.
 * We pick the first original token from the expression and query
 * LSP hover at that position. *)
let get_type_of_expr (e : G.expr) =
  let toks =
    AST_generic_helpers.ii_of_any (G.E e)
    |> List.filter Tok.is_origintok
  in
  match toks with
  | tok :: _ -> with_file_open tok type_at_tok
  | [] -> None

let init ?(lang = Lang.Ocaml) ?(expr = false) ?(roots = []) () =
  if !debug then UCommon.pr2
    (spf "LSP_client: INIT (lang=%s)" (Lang.show lang));
  let root = find_project_root lang roots in
  let conn = connect_server ~root lang in
  global := { conn = Some conn; last_uri = ""; lang };
  Core_hooks.get_type := get_type;
  if expr then Core_hooks.get_type_of_expr := get_type_of_expr;
  Stack_.push (fun () ->
      if !debug then UCommon.pr2 "LSP_client: CLOSING";
      send_request Lsp.Client_request.Shutdown conn |> ignore;
      ignore (Unix.close_process (conn.ic, conn.oc))
  ) Core_hooks.exit;
  ()

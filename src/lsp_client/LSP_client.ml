(* Yoann Padioleau, Claude Code
 *
 * Copyright (C) 2020 r2c
 * Copyright (C) 2026 Yoann Padioleau
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
 *  - Rust: rust-analyzer (needs Cargo.toml; project root auto-detected)
 *  - Python: ty or pyright (needs pyproject.toml; project root auto-detected)
 *  - TypeScript: typescript-language-server (needs tsconfig.json; project root auto-detected)
 *  - JavaScript: typescript-language-server (same as TypeScript; needs tsconfig.json)
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
      Some (Bytes.to_string buf)
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
  lsp_lang: LSP_lang.t;
}

let lsp_lang_of_lang lang =
  match lang with
  | Lang.Ocaml -> LSP_ocaml.lsp_lang
  | Lang.C | Lang.Cpp -> LSP_c.lsp_lang lang
  | Lang.Go -> LSP_go.lsp_lang
  | Lang.Rust -> LSP_rust.lsp_lang
  | Lang.Python -> LSP_python.lsp_lang
  | Lang.Ts | Lang.Js -> LSP_typescript.lsp_lang lang
  | lang ->
      failwith (spf "LSP_client: unsupported language: %s" (Lang.show lang))

let global = ref {
  conn = None;
  last_uri = "";
  lang = Lang.Ocaml;
  lsp_lang = LSP_ocaml.lsp_lang;
}

let debug = ref false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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
let find_project_root (lsp_lang : LSP_lang.t) (roots : string list) =
  let marker = lsp_lang.project_root_marker in
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

(* Read a response by ID, returning the raw JSON without typed deserialization.
 * Used for Initialize where we don't need the result and some servers
 * return capabilities the ocaml-lsp library can't parse. *)
let rec read_response_raw id conn =
  let res = Io.read conn.ic in
  match res with
  | None -> failwith "LSP_client: no answer from server"
  | Some packet ->
     (match packet with
     | Jsonrpc.Packet.Notification _ ->
            read_response_raw id conn
     | Jsonrpc.Packet.Response { Jsonrpc.Response.id = id2; result } ->
         if not (Jsonrpc.Id.equal id2 id)
         then failwith (spf "LSP_client: id mismatch: got %s, expected %s"
                          (Dumper.dump id2) (Dumper.dump id));
         (match result with
         | Ok json -> json
         | Error err ->
             let json = Jsonrpc.Response.Error.yojson_of_t err in
             let s = Yojson.Safe.pretty_to_string json in
             failwith (spf "LSP_client: server error: %s" s)
         )
     | Jsonrpc.Packet.Request { Jsonrpc.Request.id = srv_id; _ } ->
            let resp = Jsonrpc.Response.ok srv_id `Null in
            Io.write conn.oc (Jsonrpc.Packet.Response resp);
            read_response_raw id conn
     | Jsonrpc.Packet.Batch_response _
     | Jsonrpc.Packet.Batch_call _ ->
            failwith "LSP_client: unexpected batch packet"
     )

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
     | Jsonrpc.Packet.Request { Jsonrpc.Request.id = srv_id; _ } ->
            (* Server-initiated request (e.g. window/workDoneProgress/create).
             * Respond with null so the server can proceed, then keep reading. *)
            let resp = Jsonrpc.Response.ok srv_id `Null in
            Io.write conn.oc (Jsonrpc.Packet.Response resp);
            read_response (id, req) conn
     | Jsonrpc.Packet.Batch_response _
     | Jsonrpc.Packet.Batch_call _ ->
            failwith "LSP_client: unexpected batch packet"
     )

(*****************************************************************************)
(* Hover query *)
(*****************************************************************************)

let type_at_tok tk (uri : DocumentUri.t) conn =
  let lsp_lang = !global.lsp_lang in
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
            let s = lsp_lang.clean_hover s in
            if !debug then UCommon.pr2 (spf "CLEANED hover: [%s]" s);
            let ty =
              try lsp_lang.parse_type s
              with exn ->
                  if !debug then
                    UCommon.pr2_gen ("Exn parse_type_string", s, exn);
                  Exception.catch_and_reraise exn
            in
            if !debug then UCommon.pr2_gen ty;
            Some ty
      | _ ->
            failwith "LSP_client: hover response not a MarkupContent"
      )

(*****************************************************************************)
(* Server warmup *)
(*****************************************************************************)

(* Drain notifications after didOpen until the server goes idle.
 * Some servers (e.g. typescript-language-server) send diagnostics
 * notifications while they load imported modules; we wait until
 * no more data arrives for [idle_timeout] seconds.
 * This is lighter than wait_for_server_ready (no progress tracking). *)
let wait_for_open_ready conn =
  if !debug then UCommon.pr2 "LSP_client: waiting after didOpen...";
  let start = Unix.gettimeofday () in
  let max_seconds = 10.0 in
  let idle_timeout = 2.0 in
  let finished = ref false in
  while not !finished do
    let elapsed = Unix.gettimeofday () -. start in
    if elapsed > max_seconds then
      finished := true
    else begin
      let ready, _, _ =
        Unix.select [Unix.descr_of_in_channel conn.ic] [] [] idle_timeout
      in
      if List.length ready =|= 0 then
        (* Server went quiet — ready for hover queries *)
        finished := true
      else begin
        let packet = Io.read conn.ic in
        match packet with
        | None -> finished := true
        | Some (Jsonrpc.Packet.Request { Jsonrpc.Request.id = srv_id; _ }) ->
            let resp = Jsonrpc.Response.ok srv_id `Null in
            Io.write conn.oc (Jsonrpc.Packet.Response resp)
        | Some (Jsonrpc.Packet.Notification _) -> ()
        | Some _ -> ()
      end
    end
  done;
  if !debug then
    UCommon.pr2 (spf "LSP_client: didOpen ready (%.1fs)"
                   (Unix.gettimeofday () -. start))

(* Drain LSP messages until all work-done-progress tokens have ended.
 * rust-analyzer sends window/workDoneProgress/create requests followed
 * by $/progress notifications with kind begin/report/end.  We track
 * active tokens and return once all have ended (or after a timeout).
 *
 * Timeout is a safety net — we use Unix.select with a 30s ceiling per
 * read, and bail after 60s total. *)
let wait_for_server_ready conn =
  if !debug then UCommon.pr2 "LSP_client: waiting for server to be ready...";
  let active_tokens = Hashtbl.create 4 in
  let start = Unix.gettimeofday () in
  let max_seconds = 120.0 in
  let idle_timeout = 5.0 in
  let finished = ref false in
  (* We need at least one progress begin before we start tracking *)
  let seen_any_begin = ref false in
  while not !finished do
    let elapsed = Unix.gettimeofday () -. start in
    if elapsed > max_seconds then begin
      if !debug then UCommon.pr2 "LSP_client: warmup timeout, proceeding";
      finished := true
    end else begin
      (* Use select to avoid blocking forever if the server goes quiet *)
      let ready, _, _ =
        Unix.select [Unix.descr_of_in_channel conn.ic] [] [] idle_timeout
      in
      if List.length ready =|= 0 then begin
        (* No data for idle_timeout seconds *)
        if !seen_any_begin && Hashtbl.length active_tokens =|= 0 then begin
          (* All progress tokens ended — server is ready *)
          finished := true
        end else if not !seen_any_begin then begin
          (* Never saw any progress — server may already be indexed *)
          if !debug then UCommon.pr2 "LSP_client: no progress seen, assuming ready";
          finished := true
        end
      end else begin
        let packet = Io.read conn.ic in
        match packet with
        | None -> finished := true
        | Some (Jsonrpc.Packet.Request { Jsonrpc.Request.id = srv_id; _ }) ->
            (* Respond to server requests like window/workDoneProgress/create *)
            let resp = Jsonrpc.Response.ok srv_id `Null in
            Io.write conn.oc (Jsonrpc.Packet.Response resp)
        | Some (Jsonrpc.Packet.Notification
                  { Jsonrpc.Notification.method_ = "$/progress"; params = Some params }) ->
            (* Track begin/end of progress tokens *)
            (try
              let json = Jsonrpc.Structured.yojson_of_t params in
              (* Extract "token" and "value.kind" from the JSON.
               * Token can be a string or integer, so stringify via Yojson. *)
              let open Yojson.Safe.Util in
              let token = Yojson.Safe.to_string (json |> member "token") in
              let kind = json |> member "value" |> member "kind" |> to_string in
              if !debug then
                UCommon.pr2 (spf "LSP_client: progress %s: %s" token kind);
              (match kind with
               | "begin" ->
                   seen_any_begin := true;
                   Hashtbl.replace active_tokens token true
               | "end" ->
                   Hashtbl.remove active_tokens token;
                   if !seen_any_begin && Hashtbl.length active_tokens =|= 0 then
                     finished := true
               | _other -> ())
            with _exn -> ())
        | Some (Jsonrpc.Packet.Notification _) -> ()
        | Some _ -> ()
      end
    end
  done;
  if !debug then
    UCommon.pr2 (spf "LSP_client: server ready (%.1fs)"
                   (Unix.gettimeofday () -. start))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let connect_server (caps : < Cap.exec ; .. >) ~root (lsp_lang : LSP_lang.t) =
  (* the PWD of the server process is used to look for the .cmt so
   * run this program from the project you want to analyze *)
  let cmd = lsp_lang.server_cmd (caps :> < Cap.exec >) in
  let ic, oc = CapExec.open_process caps#exec cmd in
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
  (* We don't use the InitializeResult, and some servers (e.g. ty) return
   * capabilities that the ocaml-lsp library can't deserialize.  So we
   * read the raw response and skip typed parsing. *)
  let _res = read_response_raw id conn in
  if !debug then UCommon.pr2_gen _res;
  let notif = Lsp.Client_notification.Initialized in
  send_notif notif conn;
  (* Some servers (e.g. rust-analyzer) need time to load the project
   * before they can answer hover queries.  We drain messages until
   * all work-done-progress tokens have ended, or a timeout. *)
  if lsp_lang.needs_warmup then
    wait_for_server_ready conn;
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
              ~languageId:!global.lsp_lang.language_id
          )
        )
      in
      send_notif notif conn;
      (* Some servers (e.g. typescript-language-server) need time to
       * load imported modules after didOpen before hover works.
       * We drain incoming notifications (diagnostics, etc.) with a
       * short idle timeout so the server can finish processing. *)
      if !global.lsp_lang.needs_warmup then
        wait_for_open_ready conn;
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

let init (caps : < Cap.exec ; .. >) ?(lang = Lang.Ocaml) ?(expr = false) ?(roots = []) () =
  if !debug then UCommon.pr2
    (spf "LSP_client: INIT (lang=%s)" (Lang.show lang));
  let lsp_lang = lsp_lang_of_lang lang in
  let root = find_project_root lsp_lang roots in
  let conn = connect_server caps ~root lsp_lang in
  global := { conn = Some conn; last_uri = ""; lang; lsp_lang };
  Core_hooks.get_type := get_type;
  if expr then Core_hooks.get_type_of_expr := get_type_of_expr;
  Stack_.push (fun () ->
      if !debug then UCommon.pr2 "LSP_client: CLOSING";
      send_request Lsp.Client_request.Shutdown conn |> ignore;
      ignore (CapExec.close_process caps#exec (conn.ic, conn.oc))
  ) Core_hooks.exit;
  ()

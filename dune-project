(lang dune 3.7)

;; This file is used both to configure 'dune' as usual, and
;; to autogenerate the osemgrep.opam for 'opam'.

;; classic dune-project settings
(name osemgrep)
(using menhir 2.1)
; disable mangling of workspace root, to preserve debugging information
; needed to support the earlybird debugger
; See https://dune.readthedocs.io/en/stable/dune-files.html#map-workspace-root
(map_workspace_root false)

;; Opam package declarations for public libraries and public executables
;; defined in various dune files.
;; See the documentation for declaring opam packages for more info:
;; https://dune.readthedocs.io/en/stable/opam.html#opam-generation
;;
;; Type 'make osemgrep.opam' to regenerate osemgrep.opam
(generate_opam_files)

;; set here so the semgrep package below can use it and we can easily bump it
(version 2.0.0)

;; Default attributes of opam packages
(source (github aryx/osemgrep))
(homepage "https://semgrep.dev")
(maintainers "Semgrep authors")
(authors "Semgrep authors")

;; OSemgrep package
;; coupling: if you modify this package, type 'make osemgrep.opam'
(package (name osemgrep)
  (allow_empty) ; needed to avoid a 'dune build' error
  (synopsis "Like grep but for code: fast and syntax-aware semantic code pattern for many languages")
  (description "\
OSemgrep is like grep but for searching patterns at the AST level while
leveraging semantic information (e.g., types, control flow) for powerful
matching.

For more information see https://semgrep.dev
"
  )
  (maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
  (authors "Yoann Padioleau <yoann.padioleau@gmail.com>")
  (license "LGPL-2.1-only")

; note: these are build dependencies. Development-only deps are in 'dev/*.opam'.

; note: vendored libs are listed here so we pull in their opam dependencies
; (e.g., ocaml-tree-sitter-core -> tsort). They are pinned in
; semgrep.opam.template to the local version, so we don't have to worry about
; publishing to opam. If you want to add another, add it below and pin it there
;
; coupling: because of the way we need to statically link on macOS, if you add a
; library here, you'll probably need to update src/main/flags.sh if this library
; has some C stubs.
;
; coupling: if you add a dependency here and the nix workflow fails, you may
; need to manually trigger the "nix-weekly-update-flake" workflow
  (depends
    ; core deps
    (ocaml (>= 4.13.0))
    (dune (>= 2.7.0 ))
    base
    ; parser generators
    (menhir (= 20230608))
    ; vendored in libs/ocaml-tree-sitter-core
    tree-sitter
    ; standard libraries
    ; We don't use Base directly. If it's being used indirectly
    ; (is it, though?), we need a version that doesn't register
    ; rogue exception printers.
    ; See https://github.com/janestreet/base/issues/146
    (base (>= v0.15.1))
    fpath
    bos
    fileutils
    ; unit testing (note that testo is vendored in libs/)
    testo
    alcotest
    alcotest-js
    alcotest-lwt
    calendar
    ; text and terminal formating
    fmt
    ocolor
    ANSITerminal
    terminal_size
    ; Unicode - used via python-str-repr
    uucp
    uutf
    ; logging
    logs
    ; JSON/YAML/XML
    atdgen
    (yojson ( >= 2.0.0 ))
    yaml
    xmlm
    (sarif ( >= 0.3.0 ))
    ; CLI
    cmdliner
    ; PPX
    ppxlib
    (ppx_deriving ( >= 6.0.3 ))
    ppx_deriving_yojson
    ppx_hash
    ppx_inline_test
    (ppx_sexp_conv ( = v0.16.0))
    ppx_expect
    (visitors (= 20210608))
    ; regexps
    re
    pcre
    ; we're currently "vendoring" pcre2 in libs/pcre2
    pcre2
    ; misc
    ocamlgraph
    parmap
    semver
    (timedesc (>= 2.0.0)) ; used via git-wrapper
    (lsp (= 1.20.1))
    git
    ; tracing
    trace
    memtrace
    ppx_trace
    (opentelemetry (= 0.10))
    ; needed by opentelemetry-client-ocurl but the latest, 0.9.2
    ; does not work under windows (see https://github.com/ygrek/ocurl/issues/77)
    ; hence the pin to 0.9.1
    (ocurl (= 0.9.1))
    opentelemetry-client-ocurl
    ambient-context-lwt
    conf-libcurl
    ; web stuff
    uri
    (uuidm (>= 0.9.9))
    (cohttp (= 6.0.0)) ; 6.0.0 for better error handling
    cohttp-lwt-unix
    cohttp-lwt-jsoo
    tls-lwt ; needed for TLS support in the cohttp HTTP client (only TLS 1.3 seems to work)
    emile
    (digestif (>= 1.0.0))
    (tls-mirage (= 0.17.3)) ; force older version of tls-mirage to make windows work
    ; concurrency
    lwt
    lwt_ppx
    (conf-libev (<> :os win32)) ; for lwt, to get better FD perf
    ; jsoo
    (js_of_ocaml (= 5.8.2))
    (js_of_ocaml-compiler (= 5.8.2))
    (js_of_ocaml-ppx (= 5.8.2))
    (js_of_ocaml-lwt (= 5.8.2))
    integers_stubs_js
    ;TODO: this is for secrets and should be in ../dune-project but doing so
    ; slows down a lot our CI because it pulls dependencies that seem
    ; to cause the removal and recompilation of many packages (e.g., tls) and
    ; make our GHA opam cache useless. We might need to install here instead
    ; packages that would avoid this effect.
    git-unix
  )
)

;; need to provide also a semgrep package otherwise have to rename all
;; semgrep.xxx packages
(package (name semgrep)
  (synopsis "XXX")
  (description "XXX")
)

;; Other osemgrep-specific packages (sorted alphabetically).
;; Packages for semgrep-libs/ and semgrep-pfff-langs/ are defined
;; in their own dune-project files.

(package
  (name aliengrep)
  (synopsis "Generic Semgrep-like pattern search backed by PCRE")
  (description "Generic Semgrep-like pattern search backed by PCRE")
)

(package (name otarzan)
  (synopsis "XXX")
  (description "XXX")
)

(package (name pfff-lang_GENERIC-analyze)
  (synopsis "XXX")
  (description "XXX")
)
(package (name pfff-lang_GENERIC-naming)
  (synopsis "XXX")
  (description "XXX")
)

(package
  (name spacegrep)
  (synopsis "Universal program parser and matcher")
  (description "\
Uses indentation and standard braces to determine a crude parse tree
"
  )
  (maintainers "martin@semgrep.com")
  (authors "Semgrep authors")
  (license "LGPL-2.1-only")

  (depends
    alcotest
    atdgen
    ANSITerminal
    cmdliner
    (dune (>= 2.1))
  )
)

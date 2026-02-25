# OSemgrep

OSemgrep is a fork of [Semgrep](https://semgrep.dev) keeping just the OCaml
part of the project (no Python wrapper). The focus is on Jsonnet as the primary
rules format (YAML still supported), and on experimenting with new features such
as LSP/SCIP integration for advanced semantic search (e.g., using type
information), with particular attention to improving OCaml and C support.

## Building

### Prerequisites

OCaml 4.14.2 (via opam), dune, gcc, git, and a few C libraries.

On Ubuntu/Debian:
```bash
apt-get install build-essential pkg-config opam \
                libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev
```

### Quick start

```bash
git clone --recurse-submodules https://github.com/aryx/osemgrep
cd osemgrep
./configure     # installs opam deps and sets up tree-sitter (run infrequently)
make            # routine build
make test       # run tests
```

### tree-sitter

OSemgrep uses [tree-sitter](https://tree-sitter.github.io/tree-sitter/) for
parsing. Two installation paths are supported:

**Option A — system tree-sitter (preferred on supported distros)**

If your system has tree-sitter >= 0.20, `make setup` detects and uses it
automatically via `pkg-config`. To let opam manage the dependency:

```bash
opam install ./packages/conf-tree-sitter.opam
./configure
make
```

| Distro | Package | Version |
|---|---|---|
| Ubuntu 24.04 | `libtree-sitter-dev` | 0.20.9 |
| Fedora 40+ | `tree-sitter-devel` | 0.22.x |
| Alpine 3.20+ | `tree-sitter-dev` | 0.22.x |
| macOS (Homebrew) | `tree-sitter` | 0.24.x |

**Option B — local build (default fallback)**

If no compatible system tree-sitter is found, `make setup` automatically
downloads and builds tree-sitter 0.22.6 locally. No extra steps needed.

To force a local build even when a system version is present:
```bash
FORCE_LOCAL_TREE_SITTER=1 ./scripts/setup-tree-sitter.sh
```

### Docker

A reference build using Ubuntu is provided:

```bash
docker build -t osemgrep .
```

## Usage

```bash
osemgrep --help
osemgrep scan --config semgrep.jsonnet .
```

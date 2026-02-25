# Build osemgrep and semgrep libs (e.g., commons, gitignore, paths),
# which are used by my codemap/codegraph/efuns/syncweb/... projects,
# with OCaml 4.14.2 (or --build-arg OCAML_VERSION=5.2.1) via OPAM on
# Ubuntu Linux.

FROM ubuntu:22.04
# alt: 24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf git wget curl

# Setup OPAM and OCaml
RUN apt-get install -y opam
RUN opam init --disable-sandboxing -y # (disable sandboxing due to Docker)
# Tell opam to run the system package manager automatically without prompting.
# Without this, opam's depext prompt defaults to 'n' in non-interactive mode.
RUN opam option depext-run-installs=true
ARG OCAML_VERSION=4.14.2
RUN opam switch create ${OCAML_VERSION} -v

# pkg-config is needed by opam to locate C libraries; not installed by depext
RUN apt-get install -y pkg-config

# Let's go
WORKDIR /src

# Install dependencies
COPY osemgrep.opam configure Makefile ./
COPY scripts/setup-tree-sitter.sh ./scripts/
COPY ./libs/ocaml-tree-sitter-core/ ./libs/ocaml-tree-sitter-core/
#note: configure will just internally run 'make setup'
RUN ./configure

# Full build
COPY . .
RUN eval $(opam env) && make && make dune-build-all
RUN eval $(opam env) && dune install

# Testing
RUN eval $(opam env) && osemgrep --help
RUN eval $(opam env) && make test

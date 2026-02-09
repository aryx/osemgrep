# Build osemgrep and OCaml libs (for codemap/codegraph/efuns/syncweb/...)
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
ARG OCAML_VERSION=4.14.2
RUN opam switch create ${OCAML_VERSION} -v

# Let's go
WORKDIR /src

#coupling: alt: make install-deps-UBUNTU-for-semgrep-core
RUN apt-get install -y pkg-config libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev

# Install dependencies
COPY semgrep.opam dune-project configure Makefile ./
COPY ./scripts ./scripts
COPY ./libs/ocaml-tree-sitter-core/ ./libs/ocaml-tree-sitter-core/
COPY ./dev ./dev
#note: configure will just internally run 'make setup'
RUN ./configure

# Full build
COPY . .

RUN eval $(opam env) && make && make dune-build-all
RUN eval $(opam env) && dune install

RUN eval $(opam env) && osemgrep --help
RUN eval $(opam env) && make test

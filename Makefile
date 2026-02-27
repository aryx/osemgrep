###############################################################################
# Prelude
###############################################################################

# Many targets in this Makefile assume some commands have been run before to
# setup the correct build environment supporting the different languages
# used for OSemgrep development:
#  - for OCaml: 'ocamlc' and 'ocamlopt' (4.13+), 'dune', 'opam'
#  - for C: 'gcc', 'ld', 'pkg-config', but also some C libs like PCRE, gmp
#
# You will also need obviously 'make', but also 'git', and many other
# common dev tools (e.g., 'docker', 'bash').
#
# Run first (checks prerequisites and installs all dependencies):
#
#     $ ./configure    # or: make setup
#
# Then to compile osemgrep simply type:
#
#     $ make
#
# See INSTALL.md for more information
# See also https://semgrep.dev/docs/contributing/contributing-code/

# Most of the targets in this Makefile should work equally under
# Linux (Alpine, Ubuntu, Arch), macOS (x86 and arm64), Windows (WSL, Cygwin),
# and from a Dockerfile.
# The main exceptions are the install-deps-XXX-yyy targets below.

# If you really have to use platform-specific commands or flags, try to use
# macros like the one below to have a portable Makefile.
#
#     # To select commands with different usage under GNU/Linux and *BSD/Darwin
#     ifeq ($(shell uname -s),Linux)
#       LINUX = true
#     else
#       LINUX = false
#     endif
#     ifeq ($(LINUX),true)
#       SED = sed -i -e
#     else
#       SED = sed -i ''
#     endif

ifeq ($(shell uname -o),Cygwin)
  EXE = .exe
endif

###############################################################################
# Build (and clean) targets
###############################################################################

# Set environment variables used by dune files to locate the
# C headers and libraries of the tree-sitter runtime library.
# This file is created by scripts/setup-tree-sitter.sh (run via 'make setup').
#
# Because of these required environment variables, we can't call dune directly
# to build osemgrep, unless you manually execute first
#  `source ./tree-sitter-config.sh`
#
# I use '-include' and not 'include' because before 'make setup' this file does
# not exist but we still want 'make setup' to succeed
-include tree-sitter-config.mk

# First (and default) target.
# Minimal build of the osemgrep executable. Intended for the docker build.
# If you need other binaries, look at the build-xxx rules below.
.PHONY: default
default: 
	dune build _build/install/default/bin/osemgrep$(EXE)

build-docker:
	docker build -t osemgrep .
build-docker-ocaml5:
	docker build -t osemgrep --build-arg OCAML_VERSION=5.2.1 .

build-otarzan:
	dune build _build/install/default/bin/otarzan
build-ojsonnet:
	dune build _build/install/default/bin/ojsonnet
build-pfff:
	dune build _build/install/default/bin/pfff

# This is an example of how to build one of those parse-xxx ocaml-tree-sitter binaries
build-parse-cairo:
	dune build _build/install/default/bin/parse-cairo

.PHONY: clean
clean:
	dune clean

# ???? What does this build?
# This takes a long time
.PHONY: build-semgrep-jsoo
build-semgrep-jsoo:
	dune build js --profile=release

# ???? What does this build?
# Build Semgrep JS w/debug symbols, no mangling and source maps
.PHONY: build-semgrep-jsoo-debug
build-semgrep-jsoo-debug:
	dune build js --profile=dev

# Compile the JavaScript version of semgrep that runs in the browser.
# Please make it be just 'make -C js build' if you know what's going on.
.PHONY: build-js
build-js:
	$(MAKE) build-semgrep-jsoo-debug
	$(MAKE) build-js-artifacts

# ???? What does this build? Why doesn't it include the dune build?
.PHONY: build-js-artifacts
build-js-artifacts:
	make -C js build -j `nproc`

# Remove from the project tree everything that's not under source control
# and was not created by 'make setup'.
nuke: clean
# We still need to keep the nonempty opam files in git for
# 'make setup', so we should only remove the empty opam files.
# This removes the gitignored opam files.
	git clean -fX *.opam

###############################################################################
# Install targets
###############################################################################

# Install osemgrep on a developer's machine with opam installed.
# This should *not* install the open-source libraries that we maintain
# as part of the osemgrep project.
.PHONY: install
install:
	echo TODO dune install osemgrep

.PHONY: uninstall
uninstall:
	echo TODO dune uninstall osemgrep

###############################################################################
# Test target
###############################################################################

#coupling: this is run by .github/workflow/docker.yml
.PHONY: test
test: core-test

.PHONY: core-test
core-test:
	$(MAKE) build-core-test
# The following command ensures that we can call 'test.exe --help'
# from the directory of the checkout
	./test --help 2>&1 >/dev/null
	./scripts/run-core-test

# Only (re-)run the failed tests
.PHONY: retest
retest:
	$(MAKE) build-core-test
	./test run --lazy

# Please keep this standalone target.
# We want to rebuild the tests without re-running all of them.
# This is for working on one or a few specific test cases.
# It rebuilds the test executable which can then be called with
# './test <filter>' where <filter> selects the tests to run.
.PHONY: build-core-test
build-core-test:
	dune build _build/default/src/tests/test.exe

#coupling: this is run by .github/workflow/tests.yml
.PHONY: core-test-e2e
core-test-e2e:
	SEMGREP_CORE=$(PWD)/bin/semgrep-core$(EXE) \
	$(MAKE) -C interfaces test

###############################################################################
# External dependencies installation targets
###############################################################################

# **************************************************
# Platform-independent dependencies installation
# **************************************************

# We need to install all the dependencies in a single 'opam install'
# command so as to detect conflicts.
#
# WEIRD: if you use ./libs/ocaml-tree-sitter-core/ instead of the full
# path, then recent versions of opam crash with a 'git ls-files fatal error'
# about some 'libs/ocaml-tree-sitter-core/../../.git/...' not being a git repo.
#
# packages/conf-tree-sitter.opam is intentionally excluded: it is an optional
# package for systems with a compatible system-installed tree-sitter (>= 0.20).
# Install it manually with 'opam install ./packages/conf-tree-sitter.opam' 
# if desired.
REQUIRED_DEPS = ./osemgrep.opam ./libs/ocaml-tree-sitter-core/tree-sitter.opam

# This will fail if osemgrep.opam isn't up-to-date (in git),
# and dune isn't installed yet. You can always install dune with
# 'opam install dune' to get started.
osemgrep.opam: dune-project
	dune build $@
# Foolproofing
	chmod a-w osemgrep.opam

###############################################################################
# Developer targets
###############################################################################

# Install all dependencies needed to build osemgrep.
# opam's depext support (enabled by default since opam 2.1) installs the
# required system packages (pcre, gmp, libev, curl, ...) portably across
# Linux distros and macOS without platform-specific targets.
# As a developer you should not need to run this frequently, only when
# important dependencies change.
.PHONY: setup
setup:
# Configure tree-sitter: uses system library if available (via pkg-config),
# otherwise downloads and builds tree-sitter from source.
	./scripts/setup-tree-sitter.sh
	# --confirm-level=unsafe-yes is needed to auto-confirm the depext prompt
	# ("let opam run your package manager?") in non-interactive environments.
	# -y alone does not cover this prompt. See https://github.com/ocaml/opam/issues/4814
	opam install --confirm-level=unsafe-yes --deps-only $(REQUIRED_DEPS)

# Install optional development dependencies in addition to build dependencies.
.PHONY: dev-setup
dev-setup:
	$(MAKE) setup
	opam install -y --deps-only merlin

# Update and rebuild everything within the project.
.PHONY: rebuild
rebuild:
	git submodule update --init
	-$(MAKE) clean
	$(MAKE) build

# Same as 'make clean' but may remove additional files, such as external
# libraries installed locally.
#
# Specifically, this removes all files that are git-ignored. New source files
# are preserved, so this command is considered safe.
#
.PHONY: gitclean
gitclean:
	git clean -dfX
	git submodule foreach --recursive git clean -dfX

# Run utop with all the semgrep-core libraries loaded.
.PHONY: utop
utop:
	dune utop

# This is for tools/hello_script.ml so it can leverage the semgrep libs
# (e.g., commons) by installing them in ~/.opam/.../
.PHONY: install-semgrep-libs
install-semgrep-libs: # osemgrep.opam
	dune build
	dune install

# for codemap/codegraph/efuns/...
dune-build-all:
	dune build

.PHONY: dump
dump:
	_build/default/tests/test.bc -dump_ast tests/lint/stupid.py

# for ocamldebug
core-bc:
	dune build _build/install/default/bin/semgrep-core.bc
	dune build _build/install/default/bin/osemgrep.bc
test-bc:
	dune build _build/default/src/tests/test.bc
# The bytecode version of semgrep-core needs dlls for tree-sitter
# stubs installed into ~/.opam/<switch>/lib/stublibs to be able to run.
install-deps-for-semgrep-core-bc: setup
	dune build @install # Generate the treesitter stubs for below
	dune install # Needed to install treesitter_<lang> stubs for use by bytecode

# Run perf benchmarks
# Running this will reset your `semgrep` command to point to your local version
# For more information, see "Reproducing the CI benchmarks" in perf/README.md
.PHONY: perf-bench
perf-bench:
	scripts/run-benchmarks.sh

###############################################################################
# Dogfood!
###############################################################################
# There are a few places where we currently dogfood Semgrep:
#
# - in this Makefile with 'make check' below, which tests semgrep in PATH
#   and with 'make check_with_docker' which tests semgrep Docker image,
#   and where we use semgrep.jsonnet in both targets
#
# - in pre-commit in .pre-commit-config.yaml which tests the semgrep
#   Docker image used in a pre-commit 'language: docker_image' context,
#   as well as semgrep official pre-commit hooks in .pre-commit-hooks.yaml
#   in a 'language: python' context (which itself uses setup.py to install semgrep),
#   with semgrep.jsonnet but also with p/python and p/bandit rulesets.
#
# - in circle CI in .circle/config.yml which uses the Docker image
#   and where we use semgrep.jsonnet
#
# - in Github Actions (GHA) in .github/workflows/semgrep.yml where
#   we use semgrep-actions and the App to get the rules
#
# Note that many of those places use semgrep.jsonnet and so would report
# the same findings, but they are useful anyway to test all the different
# places where you can plug semgrep (Makefile, pre-commit, circleCI, GHA, GHA+App).

#coupling: see also .circleci/config.yml and its 'semgrep' job
SEMGREP_ARGS=--experimental --config semgrep.jsonnet --error --strict --exclude tests
# you can add --verbose for debugging

#Dogfooding osemgrep!
.PHONY: check
check:
	./bin/osemgrep$(EXE) $(SEMGREP_ARGS)

check_for_emacs:
	./bin/osemgrep$(EXE) $(SEMGREP_ARGS) --emacs --quiet

DOCKER_IMAGE=semgrep/semgrep:develop

# If you get parsing errors while running this command, maybe you have an old
# cached version of the docker image. You can invalidate the cache with
#   'docker rmi semgrep/semgrep:develop`
check_with_docker:
	docker run --rm -v "${PWD}:/src" $(DOCKER_IMAGE) semgrep $(SEMGREP_ARGS)

###############################################################################
# Pad's targets
###############################################################################

pr:
	git push origin `git rev-parse --abbrev-ref HEAD`
	hub pull-request -b master
push:
	git push origin `git rev-parse --abbrev-ref HEAD`
merge:
	A=`git rev-parse --abbrev-ref HEAD` && git checkout master && git pull && git branch -D $$A

# see https://github.com/aryx/codegraph for information on codegraph_build
index:
	codegraph_build -lang cmt -derived_data .
# see https://github.com/aryx/codecheck for information on codecheck
check2:
	codecheck -lang ml -with_graph_code graph_code.marshall -filter 3 .
# see https://github.com/aryx/codemap for information on codemap
visual:
	codemap -screen_size 3 -filter semgrep -efuns_client efuns_client -emacs_client /dev/null .
visual2:
	codemap -screen_size 3 -filter semgrep -efuns_client efuns_client -emacs_client /dev/null src

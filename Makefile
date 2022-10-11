.PHONY: all
all: build

_opam:
	opam switch create . --empty
	opam switch set-invariant ocaml-base-compiler.4.12.1

.PHONY: dev-deps
dev-deps: _opam
	opam pin -y -n ocamlformat 0.20.1
	opam install -y ocamlformat merlin utop

.PHONY: deps
deps: _opam
	opam install -y . ./vendor/imandra-prelude ./vendor/imandra-ptime --deps-only

.PHONY: build
build: _opam
	opam exec -- dune build @install

.PHONY: install
install: build
	opam exec -- dune install

.PHONY: clean
clean: _opam
	opam exec -- dune clean

.PHONY: format
format:
	opam exec -- dune build @src/fmt --auto-promote || true

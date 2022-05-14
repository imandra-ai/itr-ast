.PHONY: build format clean

build:
	dune build

clean:
	dune clean

format:
	dune build @fmt --auto-promote

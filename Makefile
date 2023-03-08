default: format
	dune build

format:
	dune build @fmt --auto-promote

test: default
	dune test

clean:
	dune clean

.PHONY: default format test clean

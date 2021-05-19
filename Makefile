.PHONY: build release install uninstall clean test doc reindent

build:
	dune build @install

release:
	dune build --profile release @install

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest

# requires qemu-img
benchmark: build
	./benchmark.sh

# requires odoc
doc:
	dune build @doc

gh-pages:
	bash .docgen.sh

format:
	dune build @fmt --auto-promote

.PHONY: build release install uninstall clean test doc coverage reindent

build:
		jbuilder build @install --dev

release:
		jbuilder build @install

install:
		jbuilder install

uninstall:
		jbuilder uninstall

clean:
		jbuilder clean

test:
		jbuilder runtest

coverage:
	bash .coverage.sh

reindent:
	ocp-indent --syntax cstruct -i lib/*.mli
	ocp-indent --syntax cstruct -i lib/*.ml
	ocp-indent --syntax cstruct -i lib_test/*.ml
	ocp-indent --syntax cstruct -i cli/*.ml


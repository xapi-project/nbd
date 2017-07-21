.PHONY: build release install uninstall clean doc reindent

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

# requires odoc
doc:
	jbuilder build @doc

gh-pages:
	bash .docgen.sh

reindent:
	ocp-indent --syntax cstruct -i lib/*.mli
	ocp-indent --syntax cstruct -i lib/*.ml
	ocp-indent --syntax cstruct -i lib_test/*.ml
	ocp-indent --syntax cstruct -i cli/*.ml

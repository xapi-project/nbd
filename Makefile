.PHONY: build release install uninstall clean doc

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

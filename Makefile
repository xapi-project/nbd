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

# Test in a planex-buildenv, with all the programs installed that are required for interop testing:
planex-test:
	planex-buildenv run ${BUILDENV} -- bash -eux -l -c '\
sudo yum-builddep -y xapi-nbd && \
sudo yum install -y qemu-dp nbd nmap-ncat && \
export PATH=$$PATH:/usr/lib64/qemu-dp/bin/ && \
export STRICT=true && \
echo $$PATH && \
make test'

# requires qemu-img
benchmark: build
	./benchmark.sh

# requires odoc
doc:
	dune build @doc

gh-pages:
	bash .docgen.sh

reindent:
	ocp-indent --syntax cstruct -i lib/*.mli
	ocp-indent --syntax cstruct -i lib/*.ml
	ocp-indent --syntax cstruct -i lib_test/*.ml
	ocp-indent --syntax cstruct -i cli/*.ml

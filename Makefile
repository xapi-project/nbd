OBUILDOPTS=--debug+
#CONFOPTS=--enable-library-bytecode --enable-executable-bytecode
PKGNAME=nbd

LIB_NBD=nbd.cmx nbd.cmo nbd.o nbd.cmti nbd.cmi nbd.cmxa nbd.cmt nbd.a nbd.cma
LIB_NBD_LWT=lwt_mux.cmx nbd_lwt.cmx nbd_lwt.a lwt_mux.cmi nbd_lwt.cmxa \
	lwt_mux.cmo nbd_lwt.o nbd_lwt.cmti lwt_mux.cmt nbd_lwt.cma     \
	lwt_mux.o nbd_lwt.cmo nbd_lwt.cmt
LIB_NBD_UNIX=nbd_unix.cmti nbd_unix.cmxa nbd_unix.cmo nbd_unix.cma     \
	nbd_unix.cmt nbd_unix.cmx nbd_unix.cmi nbd_unix.a nbd_unix.o
FILES:=$(addprefix dist/build/lib-nbd/,$(LIB_NBD)) \
       $(addprefix dist/build/lib-nbd_lwt/,$(LIB_NBD_LWT)) \
       $(addprefix dist/build/lib-nbd_unix/,$(LIB_NBD_UNIX))

ifneq "$(DESTDIR)" ""
INSTALL_ARGS := -destdir $(DESTDIR)
endif

.PHONY: configure build install clean uninstall

all: build

configure:
	obuild $(OBUILDOPTS) configure $(CONFOPTS)

build: configure
	obuild $(OBUILDOPTS) build

install: build
	ocamlfind remove $(PKGNAME)
	ocamlfind install $(PKGNAME) $(FILES) lib/META $(INSTALL_ARGS)

clean:
	obuild clean

uninstall:
	ocamlfind remove $(PKGNAME)

Network Block Device
====================

A pure OCaml implementation of the [Network Block
Device](http://en.wikipedia.org/wiki/Network_block_device) protocol, which is a
client/server protocol for accessing block devices.

This library installs the following ocamlfind packages:

* `nbd` : core protocol parsing library
* `nbd.lwt` : `Lwt_unix` implementation 

It also installs the `nbd-tool` command line helper.

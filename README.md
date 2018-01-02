## ocaml-rocksdb -- a binding to RocksDB

This is an OCaml library to access and manipulate RocksDB databases through the
officiel C FFI available in the RocksDB distribution. Its goal is to provide an
abstraction over the C api trying to follow simple OCaml idioms instead of relying
on the level of expressivity of the original C FFI api.

For now it aims to stay compatible with RocksDB 4.5.fb (the Debian distribution version)
but will in the long run follow the latest releases available.

It aims to be fully documented and decently tested.

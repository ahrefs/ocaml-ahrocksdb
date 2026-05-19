# ocaml-ahrocksdb -- a binding to RocksDB

This is a binding to Facebook's RocksDB.

Early prototype of this library based on [orocksdb](https://github.com/domsj/orocksdb), we decided to rewrite our own binding to make use of Ctypes's stubs generators instead of the dynamic mode used in orocksdb.

It is currently based and was tested against RocksDB 5.14fb, and should work with newer versions of this library.

## Building

By default, the library links RocksDB dynamically. To produce link flags
suitable for a fully static executable, set `ROCKSDB_LINK_MODE=static` at
build time:

```
ROCKSDB_LINK_MODE=static dune build
```

This causes the discovery script to pass `--static` to `pkg-config`, which
emits all transitive C dependencies of RocksDB (`-lz`, `-lsnappy`, `-llz4`,
`-lbz2`, `-lzstd`, `-lpthread`, etc.) in addition to `-lrocksdb`. Your system
must have static archives (`librocksdb.a` and its dependencies) installed.

`pkg-config` is used when available (the `rocksdb.pc` file must be findable,
set `PKG_CONFIG_PATH` if RocksDB is installed in a non-standard prefix). If
`pkg-config` is not available or does not know about `rocksdb`, the discovery
script falls back to searching the standard system header paths and emitting
`-lrocksdb` as the sole link flag.

## API changes and contributions

While we do not plan big changes in what is already implemented, we do not guarantee the stability of these APIs.

Some APIs could definitely use improvements (moving the current configuration system to a builder-like pattern),
and some breakage may or may-not happen.

Pull requests to improve any parts of the library are however welcome, whether they are related to
tests, binding coverage, or API improvements, feel free to open an issue to discuss changes.

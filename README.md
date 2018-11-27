# ocaml-ahrocksdb -- a binding to RocksDB

This is a binding to Facebook's RocksDB.

Early prototype of this library based on [orocksdb](https://github.com/domsj/orocksdb), we decided to rewrite our own binding to make use of Ctypes's stubs generators instead of the dynamic mode used in orocksdb.

It is currently based and was tested against RocksDB 5.14fb, and should work with newer versions of this library.

## API changes and contributions

While we do not plan big changes in what is already implemented, we do not guarantee the stability of these APIs.

Some APIs could definitely use improvements (moving the current configuration system to a builder-like pattern),
and some breakage may or may-not happen.

Pull requests to improve any parts of the library are however welcome, wether they are related to
tests, binding coverage, or API improvements, feel free to open an issue to discuss changes.

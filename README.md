# ocaml-rocksdb -- a binding to RocksDB

This is a binding to Facebook's RocksDB, based on their official C APIs.
This library is based on [orocksdb](https://github.com/domsj/orocksdb).
It is currently based and was tested against RocksDB 5.8fb, and should work with newer versions of this library.

## API changes and contributions

While we do not plan big changes in what is already implemented, we do not guarantee the stability of these APIs.
Some APIs could definitely use improvements (moving the current configuration system to a builder-like pattern),
and some breakage may or may-not happen.
Pull requests to improve any parts of the library are however welcome, wether they are related to
tests, binding coverage, or API improvements, feel free to open an issue to discuss changes!

## Testimonials

We have been using this binding for a few months now and built enough confidence to open-source it as is.
We use RocksDB as a secondary storage engine to provide fast operations on various components of our web crawler.

Our RocksDB based services handle an estimated load of 50 millions writes a minute and more than sixfold this amount of reads.

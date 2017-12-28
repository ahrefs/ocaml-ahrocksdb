type options

type config = {
  parallelism_level : int option;
  compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ];
}

val default : config
val create : config:config -> options

module Options = Rocksdb_ffi.M.Options

type options = Options.options

type config = {
  parallelism_level : int option;
  compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ];
}

let apply_config options {
    parallelism_level;
    compression;
  } =
  let iter_opt o f =
    match o with
    | Some v -> f v
    | None -> ()
  in
  let (>>=) = iter_opt in
  parallelism_level >>= Options.increase_parallelism options;
  Options.set_compression options compression

let default = {
  parallelism_level = None;
  compression = `No_compression;
}

let create ~config =
  let open Ctypes in
  let t = Options.create () in
  Gc.finalise Options.destroy t;
  apply_config t config;
  t

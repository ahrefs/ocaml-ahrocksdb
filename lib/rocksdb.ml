module Ffi = Rocksdb_ffi.M

module Options = struct

  module Options = Ffi.Options

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

end

module Db = struct

  type t = Ffi.Rocksdb.t

  let open_db ?create:(create=false) ~options ~name =
    let open Ctypes in
    let errb = allocate string_opt None in
    Ffi.Options.set_create_if_missing options create;
    let handle = Ffi.Rocksdb.open_ options name errb in
    match !@ errb with
    | None -> Ok handle
    | Some err -> Error err

  let close_db t = Ffi.Rocksdb.close t

end

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
    let open Misc.Opt in
    parallelism_level >>= Options.increase_parallelism options;
    Options.set_compression options compression

  let default = {
    parallelism_level = None;
    compression = `No_compression;
  }

  let options_of_config config =
    let open Ctypes in
    let t = Options.create () in
    Gc.finalise Options.destroy t;
    apply_config t config;
    t

end

open Ctypes
module Rocksdb = Ffi.Rocksdb

module Write_options = struct

  open Rocksdb

  type t = Write_options.t

  let create ?disable_wal ?sync () =
    let open Ctypes in
    let open Misc.Opt in
    let t = Write_options.create () in
    disable_wal >>= Write_options.disable_WAL t;
    sync >>= Write_options.set_sync t;
    Gc.finalise Write_options.destroy t;
    t

end

module Read_options = struct

  open Rocksdb

  type t = Read_options.t

  let create ?verify_checksums ?fill_cache ?tailing () =
    let open Ctypes in
    let open Misc.Opt in
    let t = Read_options.create () in
    verify_checksums >>= Read_options.set_verify_checksums t;
    fill_cache >>= Read_options.set_fill_cache t;
    tailing >>= Read_options.set_tailing t;
    Gc.finalise Read_options.destroy t;
    t

end

type db = Rocksdb.db

let with_error_buffer fn =
  let errb = allocate string_opt None in
  let result = fn errb in
  match !@ errb with
  | None -> Ok result
  | Some err -> Error err

let open_db ?create:(create=false) ~options ~name =
  Ffi.Options.set_create_if_missing options create;
  with_error_buffer @@ Rocksdb.open_ options name

let close_db t = Rocksdb.close t

let put db write_options ~key ~value =
  let key_len = String.length key in
  let value_len = String.length value in
  Rocksdb.put db write_options (ocaml_string_start key) key_len (ocaml_string_start value) value_len
  |> with_error_buffer

let delete db write_options key =
  let key_len = String.length key in
  Rocksdb.delete db write_options (ocaml_string_start key) key_len
  |> with_error_buffer

let get db read_options key =
  let key_len = String.length key in
  let result_len = allocate Ffi.V.int_to_size_t 0 in
  let result = with_error_buffer @@ Rocksdb.get db read_options (ocaml_string_start key) key_len result_len in
  match result with
  | Error err -> `Error err
  | Ok result_ptr ->
    match Ctypes.is_null (to_voidp result_ptr) with
    | true -> `Not_found
    | false ->
      let result = string_from_ptr result_ptr (!@ result_len) in
      Gc.finalise (fun result_ptr -> Rocksdb.free (to_voidp result_ptr)) result_ptr;
      `Ok result

module Batch = struct

  open Rocksdb

  type t = Batch.t

  let create () =
    let open Ctypes in
    let t = Batch.create () in
    Gc.finalise Batch.destroy t;
    t

  let put batch ~key ~value =
    let key_len = String.length key in
    let value_len = String.length value in
    Batch.put batch (ocaml_string_start key) key_len (ocaml_string_start value) value_len

  let write db write_options batch = Rocksdb.write db write_options batch |> with_error_buffer

  let simple_write_batch db write_options elts =
    let batch = create () in
    List.iter (fun (key, value) -> put batch key value) elts;
    write db write_options batch

end

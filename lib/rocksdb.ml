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

open Ctypes
module Rocksdb = Ffi.Rocksdb

type t = Rocksdb.t
type ropts = Rocksdb.ropts
type wopts = Rocksdb.wopts

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

let init_writeoptions t =
  let wopts = Rocksdb.write_options_create t in
  Gc.finalise Rocksdb.write_options_destroy wopts;
  wopts

let put ?wopts t ~key ~value =
  let wopts = match wopts with
    | Some wopts -> wopts
    | None -> init_writeoptions t
  in
  let key_len = String.length key in
  let value_len = String.length value in
  Rocksdb.put t wopts (ocaml_string_start key) key_len (ocaml_string_start value) value_len
  |> with_error_buffer

let delete ?wopts t key =
  let wopts = match wopts with
    | Some wopts -> wopts
    | None -> init_writeoptions t
  in
  let key_len = String.length key in
  Rocksdb.delete t wopts (ocaml_string_start key) key_len
  |> with_error_buffer

let get t key =
  let ropts = Rocksdb.read_options_create t in
  let key_len = String.length key in
  let result_len = allocate Ffi.V.int_to_size_t 0 in
  let result = with_error_buffer @@ Rocksdb.get t ropts (ocaml_string_start key) key_len result_len in
  Rocksdb.read_options_destroy ropts;
  match result with
  | Error err -> `Error err
  | Ok result_ptr ->
    match Ctypes.is_null (to_voidp result_ptr) with
    | true -> `Not_found
    | false ->
      let result = string_from_ptr result_ptr (!@ result_len) in
      Gc.finalise (fun result_ptr -> Rocksdb.free (to_voidp result_ptr)) result_ptr;
      `Ok result

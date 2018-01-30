module Ffi = Rocksdb_ffi.M
module Rocksdb = Ffi.Rocksdb
open Ctypes

module Options = struct

  module Options = Ffi.Options

  type options = Options.options

  type config = {
    parallelism_level : int option;
    compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ];
    disable_compaction : bool;
    max_flush_processes : int option;
    compaction_trigger : int option;
    slowdown_writes_trigger : int option;
    stop_writes_trigger : int option;
    memtable_representation : [ `Vector ] option
  }

  let apply_config options {
      parallelism_level;
      compression;
      disable_compaction;
      max_flush_processes;
      compaction_trigger;
      slowdown_writes_trigger;
      stop_writes_trigger;
      memtable_representation;
    } =
    let open Misc.Opt in
    parallelism_level >>= Options.increase_parallelism options;
    max_flush_processes >>= Options.set_max_background_flushes options;
    compaction_trigger >>= Options.set_level0_file_num_compaction_trigger options;
    slowdown_writes_trigger >>= Options.set_level0_slowdown_writes_trigger options;
    stop_writes_trigger >>= Options.set_level0_stop_writes_trigger options;
    match memtable_representation with
    | Some `Vector -> Options.set_memtable_vector_rep options;
    | _ -> ();
    Options.set_compression options compression;
    Options.set_disable_auto_compactions options disable_compaction

  let default = {
    parallelism_level = None;
    compression = `No_compression;
    disable_compaction = false;
    max_flush_processes = None;
    compaction_trigger = None;
    slowdown_writes_trigger = None;
    stop_writes_trigger = None;
    memtable_representation = None;
  }

  let options_of_config config =
    let t = Options.create () in
    Gc.finalise Options.destroy t;
    apply_config t config;
    t

end

module Write_options = struct

  open Rocksdb

  type t = Write_options.t

  let create ?disable_wal ?sync () =
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

let compact_now db = Rocksdb.compact_range db None 0 None 0

let stats db =
  match Rocksdb.property_value db "rocksdb.stats" with
  | None -> None
  | Some stats ->
    let string = coerce (ptr char) string stats in
    Gc.finalise (fun stats -> Rocksdb.free (to_voidp stats)) stats;
    Some string

module Batch = struct

  open Rocksdb

  type t = Batch.t

  let create () =
    let t = Batch.create () in
    Gc.finalise Batch.destroy t;
    t

  let count = Batch.count

  let clear = Batch.clear

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

module Iterator = struct

  open Rocksdb

  type t = Iterator.t

  let create db read_options =
    let t = Iterator.create db read_options in
    Gc.finalise Iterator.destroy t;
    t

  let seek t key =
    let len = String.length key in
    Iterator.seek t (ocaml_string_start key) len

  let next = Iterator.next

  let get_key t =
    let result_len = allocate Ffi.V.int_to_size_t 0 in
    let result = Iterator.key t result_len in
    match Ctypes.is_null (to_voidp result) with
    | true -> raise Not_found
    | false ->
       let result_s = string_from_ptr result (!@ result_len) in
       Gc.finalise (fun result_ptr -> Rocksdb.free (to_voidp result)) result;
       result_s

  let get_value t =
    let result_len = allocate Ffi.V.int_to_size_t 0 in
    let result = Iterator.value t result_len in
    match Ctypes.is_null (to_voidp result) with
    | true -> raise Not_found
    | false ->
       let result_s = string_from_ptr result (!@ result_len) in
       Gc.finalise (fun result_ptr -> Rocksdb.free (to_voidp result)) result;
       result_s

  let is_valid = Iterator.valid

  let get t =
    if is_valid t then begin
       try
         let key = get_key t in
         let value = get_value t in
         Some (key, value)
       with
       | Not_found -> None
    end
    else None

end

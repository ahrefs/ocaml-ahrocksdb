module Ffi = Rocksdb_ffi.M
module Rocksdb = Ffi.Rocksdb
open Ctypes

module Options = struct

  module Options = Ffi.Options

  type options = Options.options

  module Filter_policy = struct

    module B = Options.FilterPolicy

    type t = B.t

    let create_bloom ~bits_per_key =
      let t = B.create_bloom bits_per_key in
      Gc.finalise B.destroy t;
      t

    let create_bloom_full ~bits_per_key =
      let t = B.create_bloom_full bits_per_key in
      Gc.finalise B.destroy t;
      t

  end

  module Tables = struct

    module Block_based = struct

      module B = Options.Tables.BlockBased

      type t = B.t

      let create ~block_size =
        let t = B.create () in
        B.set_block_size t block_size;
        Gc.finalise B.destroy t;
        t

      let set_filter_policy t filter_policy = B.set_filter_policy t filter_policy

    end

  end

  type table_format = Block_based of Tables.Block_based.t

  type config = {
    parallelism_level : int option;
    compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ];
    optimize_filters_for_hits: bool option;
    disable_compaction : bool;
    max_flush_processes : int option;
    compaction_trigger : int option;
    slowdown_writes_trigger : int option;
    stop_writes_trigger : int option;
    memtable_representation : [ `Vector ] option;
    num_levels : int option;
    write_buffer_size : int option;
    max_write_buffer_number : int option;
    min_write_buffer_number_to_merge : int option;
    target_base_file_size : int option;
    table_format : table_format option;
    max_open_files : int option;
  }

  let apply_config options {
      parallelism_level;
      compression;
      optimize_filters_for_hits;
      disable_compaction;
      max_flush_processes;
      compaction_trigger;
      slowdown_writes_trigger;
      stop_writes_trigger;
      memtable_representation;
      num_levels;
      target_base_file_size;
      table_format;
      write_buffer_size;
      max_write_buffer_number;
      max_open_files;
      min_write_buffer_number_to_merge;
    } =
    let open Misc.Opt in
    parallelism_level >>= Options.increase_parallelism options;
    optimize_filters_for_hits >>= Options.set_optimize_filters_for_hits options;
    max_flush_processes >>= Options.set_max_background_flushes options;
    compaction_trigger >>= Options.set_level0_file_num_compaction_trigger options;
    slowdown_writes_trigger >>= Options.set_level0_slowdown_writes_trigger options;
    stop_writes_trigger >>= Options.set_level0_stop_writes_trigger options;
    target_base_file_size >>= Options.set_target_file_size_base options;
    num_levels >>= Options.set_num_levels options;
    write_buffer_size >>= Options.set_write_buffer_size options;
    max_write_buffer_number >>= Options.set_max_write_buffer_number options;
    min_write_buffer_number_to_merge >>= Options.set_min_write_buffer_number_to_merge options;
    max_open_files >>= Options.set_max_open_files options;
    match memtable_representation with
    | Some `Vector -> Options.set_memtable_vector_rep options;
    | _ -> ();
    match table_format with
    | Some (Block_based config) -> Options.set_block_based_table_factory options config
    | None -> ();
    Options.set_compression options compression;
    Options.set_disable_auto_compactions options disable_compaction

  let default = {
    parallelism_level = None;
    compression = `No_compression;
    optimize_filters_for_hits = None;
    disable_compaction = false;
    max_flush_processes = None;
    compaction_trigger = None;
    slowdown_writes_trigger = None;
    stop_writes_trigger = None;
    memtable_representation = None;
    num_levels = None;
    target_base_file_size = None;
    table_format = None;
    write_buffer_size = None;
    max_write_buffer_number = None;
    min_write_buffer_number_to_merge = None;
    max_open_files = None;
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

module Flush_options = struct

  open Rocksdb

  type t = Flush_options.t

  let create ?wait () =
    let open Misc.Opt in
    let t = Flush_options.create () in
    wait >>= Flush_options.wait t;
    Gc.finalise Flush_options.destroy t;
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

type db = {
  mutable closed: bool;
  db: Rocksdb.db;
}

let close = function
  | { closed = true; _ } -> ()
  | db ->
    db.closed <- true;
    Rocksdb.close db.db

let close_db = function
  | { closed = true; _ } -> Error "trying to close a database handle already closed"
  | db ->
    db.closed <- true;
    Rocksdb.close db.db;
    Ok ()

let wrap_db db = { db; closed = false }

let unwrap_db db f =
  match db with
  | { closed = true; _ } -> Error "trying to access closed database handle"
  | { db; _ } -> f db

let with_error_buffer fn =
  let errb = allocate string_opt None in
  let result = fn errb in
  match !@ errb with
  | None -> Ok result
  | Some err -> Error err

let open_db ?create:(create=false) ~options ~name =
  Ffi.Options.set_create_if_missing options create;
  match with_error_buffer @@ Rocksdb.open_ options name with
  | Ok t ->
    let t = wrap_db t in
    Gc.finalise close t;
    Ok t
  | Error err -> Error err

let open_db_read_only ?fail_on_wal:(fail=false) ~options ~name =
  match with_error_buffer @@ Rocksdb.open_read_only options name fail with
  | Ok t ->
    let t = wrap_db t in
    Gc.finalise close t;
    Ok t
  | Error err -> Error err

let open_db_with_ttl ?create:(create=false) ~options ~name ~ttl =
  Ffi.Options.set_create_if_missing options create;
  match with_error_buffer @@ Rocksdb.open_with_ttl options name ttl with
  | Ok t ->
    let t = wrap_db t in
    Gc.finalise close t;
    Ok t
  | Error err -> Error err


let put db write_options ~key ~value =
  let key_len = String.length key in
  let value_len = String.length value in
  unwrap_db db @@ fun db ->
  Rocksdb.put db write_options (ocaml_string_start key) key_len (ocaml_string_start value) value_len
  |> with_error_buffer

let delete db write_options key =
  let key_len = String.length key in
  unwrap_db db @@ fun db ->
  Rocksdb.delete db write_options (ocaml_string_start key) key_len
  |> with_error_buffer

let get db read_options key =
  let key_len = String.length key in
  let result_len = allocate Ffi.V.int_to_size_t 0 in
  let result = unwrap_db db @@ fun db ->
    with_error_buffer @@ Rocksdb.get db read_options (ocaml_string_start key) key_len result_len
  in
  match result with
  | Error err -> `Error err
  | Ok result_ptr ->
    match Ctypes.is_null (to_voidp result_ptr) with
    | true -> `Not_found
    | false ->
      let result = string_from_ptr result_ptr (!@ result_len) in
      Gc.finalise (fun result_ptr -> Rocksdb.free (to_voidp result_ptr)) result_ptr;
      `Ok result

let flush db flush_options =
  unwrap_db db @@ fun db ->
  Rocksdb.flush db flush_options
  |> with_error_buffer

let compact_now db =
  unwrap_db db @@ fun db ->
  Ok (Rocksdb.compact_range db None 0 None 0)

let stats db =
  unwrap_db db @@ fun db ->
  match Rocksdb.property_value db "rocksdb.stats" with
  | None -> Ok None
  | Some stats ->
    let string = coerce (ptr char) string stats in
    Gc.finalise (fun stats -> Rocksdb.free (to_voidp stats)) stats;
    Ok (Some string)

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

  let write db write_options batch =
    unwrap_db db @@ fun db ->
    Rocksdb.write db write_options batch |> with_error_buffer

  let simple_write_batch db write_options elts =
    let batch = create () in
    List.iter (fun (key, value) -> put batch key value) elts;
    write db write_options batch

end

module Iterator = struct

  open Rocksdb

  type t = Iterator.t

  let create db read_options =
    unwrap_db db @@ fun db ->
    let t = Iterator.create db read_options in
    Gc.finalise Iterator.destroy t;
    Ok t

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
       result_s

  let get_value t =
    let result_len = allocate Ffi.V.int_to_size_t 0 in
    let result = Iterator.value t result_len in
    match Ctypes.is_null (to_voidp result) with
    | true -> raise Not_found
    | false ->
       let result_s = string_from_ptr result (!@ result_len) in
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

module Ffi = Rocksdb_ffi.M
module Rocksdb = Ffi.Rocksdb

open Ffi.Options

module Filter_policy = struct

  type t = FilterPolicy.t

  let create_bloom ~bits_per_key = FilterPolicy.create_bloom bits_per_key
  let create_bloom_full ~bits_per_key = FilterPolicy.create_bloom_full bits_per_key

end

module Cache = struct

  type t = Cache.t

  module LRU = struct

    let create ~size = Cache.create size
    let get_usage t = Cache.get_usage t

  end

end

module Tables = struct

  open Tables

  type format = Block_based of BlockBased.t

  module Block_based = struct

    let create ~block_size =
      let t = BlockBased.create () in
      BlockBased.set_block_size t block_size;
      Gc.finalise BlockBased.destroy t;
      Block_based t

    let set_filter_policy t filter_policy = BlockBased.set_filter_policy t filter_policy
    let set_cache_index_and_filter_blocks t b = BlockBased.set_cache_index_and_filter_blocks t b
    let set_block_cache t cache = BlockBased.set_block_cache t cache

  end

end

(** Actual configuration handling **)


type t = options

let create () =
  let t = create () in
  Gc.finalise destroy t;
  t

type config = {
    parallelism_level : int option;
    base_compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ];
    compression_by_level : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ] list;
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
    table_format : Tables.format option;
    max_open_files : int option;
    create_if_missing : bool;
    filter_policy : Filter_policy.t option;
    cache_index_and_filter_blocks : bool;
    block_cache : Cache.t option;
    trace_perf : bool;
    bulk_load : bool;
  }

let of_config {
    parallelism_level;
    base_compression;
    compression_by_level;
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
    create_if_missing;
    filter_policy;
    block_cache;
    cache_index_and_filter_blocks;
    bulk_load;
    trace_perf=_;
  } =

  let options = create () in

  (* applying options belonging to the RocksDB options object itself *)

  let open Misc.Opt in

  set_create_if_missing options create_if_missing;
  set_compression options base_compression;
  set_disable_auto_compactions options disable_compaction;

  parallelism_level                >>= increase_parallelism options;
  optimize_filters_for_hits        >>= set_optimize_filters_for_hits options;
  max_flush_processes              >>= set_max_background_flushes options;
  compaction_trigger               >>= set_level0_file_num_compaction_trigger options;
  slowdown_writes_trigger          >>= set_level0_slowdown_writes_trigger options;
  stop_writes_trigger              >>= set_level0_stop_writes_trigger options;
  target_base_file_size            >>= set_target_file_size_base options;
  num_levels                       >>= set_num_levels options;
  write_buffer_size                >>= set_write_buffer_size options;
  max_write_buffer_number          >>= set_max_write_buffer_number options;
  min_write_buffer_number_to_merge >>= set_min_write_buffer_number_to_merge options;
  max_open_files                   >>= set_max_open_files options;

  (match memtable_representation with
  | Some `Vector -> set_memtable_vector_rep options;
  | _ -> ());

  (* applying options depending on other objects allocated from within RocksDB *)

  (match table_format with
  | Some (Block_based config) -> begin
    Tables.Block_based.set_cache_index_and_filter_blocks config cache_index_and_filter_blocks;
    filter_policy >>= Tables.Block_based.set_filter_policy config;
    block_cache >>= Tables.Block_based.set_block_cache config;
    set_block_based_table_factory options config;
  end
  | None -> ());

  let len = List.length compression_by_level in
  if len > 0 then begin
    let ba = Bigarray.Array1.create Bigarray.int Bigarray.c_layout len in
    let () = List.iteri (fun i elt -> Bigarray.Array1.set ba i (Ffi.V.int_of_compression elt)) compression_by_level in
    set_compression_per_level options (Ctypes.bigarray_start Ctypes.array1 ba) len
  end;

  if bulk_load then
    prepare_for_bulk_load options;

  options

let default = {
    parallelism_level = None;
    base_compression = `No_compression;
    compression_by_level = [];
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
    create_if_missing = true;
    filter_policy = None;
    cache_index_and_filter_blocks = false;
    block_cache = None;
    bulk_load = false;
    trace_perf = false;
}

module Write_options = struct

  open Ffi.Rocksdb

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

  open Ffi.Rocksdb

  type t = Flush_options.t

  let create ?wait () =
    let open Misc.Opt in
    let t = Flush_options.create () in
    wait >>= Flush_options.wait t;
    Gc.finalise Flush_options.destroy t;
    t

end

module Read_options = struct

  open Ffi.Rocksdb

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

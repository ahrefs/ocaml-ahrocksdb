(** A binding to RocksDB

This library is a binding to Facebook's RocksDB library.
This library attempts to provide a layer on top of Rock's C FFI adapting it as
much as possible to OCaml idioms.
It aims to cover most of the C FFI in the long run, along with tests to ensure
no function is left never called and untested.

*)

type error = [ `Msg of string ]


module Options : sig

  (** High-level bindings for RocksDB open options

      This module provides a binding to the open options available in Rock's C FFIs.
      It provides a type [config] which holds the configuration options for opening a Rocksdb database.
      Configuration settings are then mapped to the appropriate FFI calls when the database is opened.

  *)

  module Filter_policy : sig

    (** Filter policy bindings

        This module is used to create a bloom filter to be set in the {!config} type.
        More information about the different bloom filter kinds available here: https://github.com/facebook/rocksdb/wiki/RocksDB-Bloom-Filter
    *)

    type t

    val create_bloom : bits_per_key:int -> t
    val create_bloom_full : bits_per_key:int -> t

  end

  module Cache : sig

    (** Block-caching facilities

        This module is used to instantiate cache objects that can be set in the {!config} type.
        See: https://github.com/facebook/rocksdb/wiki/Block-Cache
    *)

    type t

    module LRU : sig

      (** [create size] will create a new LRU cache object of size [size] (in bytes) *)
      val create : size:int -> t

    end

  end

  module Tables : sig

    type format

    (** Table format facilities bindings
        See: https://github.com/facebook/rocksdb/wiki/A-Tutorial-of-RocksDB-SST-formats
    *)

    module Block_based : sig

      val create : block_size:int -> format

    end

  end

  (** Write options *)
  module Write_options : sig

    type t

    val create : ?disable_wal:bool -> ?sync:bool -> unit -> t
    (** [create disable_wal sync] returns a new WriteOptions object to be used to
      configure write operations on a RocksDB database.
      TODO
    *)

  end

  (** Flush options *)
  module Flush_options : sig

    type t

    val create : ?wait:bool -> unit -> t
    (** [create wait] returns a new FlushOptions object to be used to
      configure Flush operations on a RocksDB database.
      TODO
     *)

  end

  (** Read options *)
  module Read_options : sig

    type t

    val create : ?verify_checksums:bool -> ?fill_cache:bool -> ?tailing:bool -> unit -> t
    (** [create verify_checksums fill_cache tailing] returns a new ReadOptions object to be used to
        configure read operations on a RocksDB database.
      TODO
     *)

  end

  (** RocksDB main configuration record *)
  type config = {
    parallelism_level : int option; (** Number of background processes used by RocksDB *)
    base_compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ]; (** Compression algorithm used to compact data at base level*)
    compression_by_level : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ] list; (** Compression algorithm used to compact data in order for each level*)
    optimize_filters_for_hits: bool option;
    disable_compaction : bool; (** Disable compaction: data will not be compressed, but manual compaction can still be issued *)
    max_flush_processes : int option; (** Number of background workers dedicated to flush *)
    compaction_trigger : int option; (** Maximum size for a file in level0 to wait for initiating compaction *)
    slowdown_writes_trigger : int option; (** TODO *)
    stop_writes_trigger : int option; (** TODO *)
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
  }

  (** default configuration, only compression is set to `Snappy, everything else is None (RocksDB defaults will apply) *)
  val default : config


end

(** Opaque database handle *)
type t

val open_db : config:Options.config -> name:string -> (t, error) result
(** [open_db options name] will return an handle to the database in case
    of success or the error returned by RocksDB in case of failure.
    [name] is the path to the database.
*)

val open_db_read_only : ?fail_on_wal:bool -> config:Options.config -> name:string -> (t, error) result
(** [open_db options name] will return a read-only handle to the database in case
    of success or the error returned by RocksDB in case of failure.
    [name] is the path to the database.
    [fail_on_wal] returns an error if write log is not empty
*)

val open_db_with_ttl : config:Options.config -> name:string -> ttl:int -> (t, error) result
(** [open_db_with_ttl options name ttl] will return an handle to the database in case
    of success or the error returned by RocksDB in case of failure.
    [name] is the path to the database.
    [ttl] Time in seconds after which a key should be removed (best-effort basis, during compaction)
*)

val put : t -> Options.Write_options.t -> key:string -> value:string -> (unit, error) result
(** [put db write_options key value] will write at key [key] the value [value], on database [db].
    Return unit on success, RocksDB reported error on error.
*)

val delete : t -> Options.Write_options.t -> string -> (unit, error) result
(** [delete db write_options key] will delete key [key] on database [db].
    Return unit on success, RocksDB reported error on error.
*)

val get : t -> Options.Read_options.t -> string -> ([ `Not_found | `Found of string ], error) result
(** [get db read_options key] will fetch key [key] on database [db].
    Returns `Ok value if the key is found, `Not_found otherwise, and `Error if a failure occurred.
*)

val flush : t -> Options.Flush_options.t -> (unit, error) result
(** [flush db flush_options] will flush all pending memtables on database [db].
    Return unit on success, RocksDB reported error on error.
*)

val compact_now : t -> (unit, error) result
(** [compact_now db] will initiate a compaction on all ranges available in database. This is an asynchronous operation, returning unit once operation is started. *)

val stats : t -> (string option, error) result
(** [stats db] will return the accumulated stats for this database handle as an optional string form *)

val get_cache_usage : t -> (int, error) result

val close_db : t -> (unit, error) result
(** [close db] explicitly closes the db handle. Any further access will raise an error *)

(** Batch processing
    RocksDB allows to batch operations through a dedicated batch object that must be fed to {!write}.
    A batch object {!Batch.batch} is a collection of operation to run on a database. (like {!Batch.put} or delete).
*)
module Batch : sig

  (** An opaque batch request must be created through {!create} and executed through {!write} *)
  type batch

  (** [create] will create a batch job to be used to batch operation on the database. *)
  val create : unit -> batch

  (** [count] number of operations in the batch object *)
  val count : batch -> int

  (** clear operations from the batch object *)
  val clear : batch -> unit

  (** [put batch key value] will take a [batch] job and stage the writing of the [key] key and [value] value in the batch job. *)
  val put : batch -> key:string -> value:string -> unit

  (** [write db write_options batch] takes a [db] handle, some [write_options] and a [batch] job and execute it on the database. *)
  val write : t -> Options.Write_options.t -> batch -> (unit, error) result

  (** A simple helper, will take a list of key_value and do a unique batch and write it to the database *)
  val simple_write_batch : t -> Options.Write_options.t -> (string * string) list -> (unit, error) result

end

module Iterator : sig

  type iterator

  val create : t -> Options.Read_options.t -> (iterator, error) result

  (** [seek iterator prefix] will set the iterator [t] in seek mode, iterating on keys starting by [prefix] *)
  val seek : iterator -> string -> unit

  (** [get iterator] will get the current key value pair on iterator [t]. Calling it multiple time in a row with no change of position results in the same pair being returned *)
  val get : iterator -> (string * string) option

  (** [next iterator] will set the iterator to the next key in the range. pair on iterator [t].
      Be mindful of the fact that you need to check if the iterator is still valid via {!is_valid}, and that according to RocksDB documentation, in prefix mode,
      you should make sure that the key is indeed starting by your prefix as your ending condition while iterating, since after finishing the range, RocksDB might return the next range after it.
      See https://github.com/facebook/rocksdb/wiki/Prefix-Seek-API-Changes#transition-to-the-new-usage
  *)
  val next : iterator -> unit

  val is_valid : iterator -> bool

end

module Perf_context : sig


  (** Perf context counters bindings
      See: https://github.com/facebook/rocksdb/wiki/Perf-Context-and-IO-Stats-Context
  *)

  type perf_context
  type counter

  module Counters : sig

    val user_key_comparison_count : counter
    val block_cache_hit_count : counter
    val block_read_count : counter
    val block_read_byte : counter
    val block_read_time : counter
    val block_checksum_time : counter
    val block_decompress_time : counter
    val get_read_bytes : counter
    val multiget_read_bytes : counter
    val iter_read_bytes : counter
    val internal_key_skipped_count : counter
    val internal_delete_skipped_count : counter
    val internal_recent_skipped_count : counter
    val internal_merge_count : counter
    val get_snapshot_time : counter
    val get_from_memtable_time : counter
    val get_from_memtable_count : counter
    val get_post_process_time : counter
    val get_from_output_files_time : counter
    val seek_on_memtable_time : counter
    val seek_on_memtable_count : counter
    val next_on_memtable_count : counter
    val prev_on_memtable_count : counter
    val seek_child_seek_time : counter
    val seek_child_seek_count : counter
    val seek_min_heap_time : counter
    val seek_max_heap_time : counter
    val seek_internal_seek_time : counter
    val find_next_user_entry_time : counter
    val write_wal_time : counter
    val write_memtable_time : counter
    val write_delay_time : counter
    val write_pre_and_post_process_time : counter
    val db_mutex_lock_nanos : counter
    val db_condition_wait_nanos : counter
    val merge_operator_time_nanos : counter
    val read_index_block_nanos : counter
    val read_filter_block_nanos : counter
    val new_table_block_iter_nanos : counter
    val new_table_iterator_nanos : counter
    val block_seek_nanos : counter
    val find_table_nanos : counter
    val bloom_memtable_hit_count : counter
    val bloom_memtable_miss_count : counter
    val bloom_sst_hit_count : counter
    val bloom_sst_miss_count : counter
    val key_lock_wait_time : counter
    val key_lock_wait_count : counter
    val env_new_sequential_file_nanos : counter
    val env_new_random_access_file_nanos : counter
    val env_new_writable_file_nanos : counter
    val env_reuse_writable_file_nanos : counter
    val env_new_random_rw_file_nanos : counter
    val env_new_directory_nanos : counter
    val env_file_exists_nanos : counter
    val env_get_children_nanos : counter
    val env_get_children_file_attributes_nanos : counter
    val env_delete_file_nanos : counter
    val env_create_dir_nanos : counter
    val env_create_dir_if_missing_nanos : counter
    val env_delete_dir_nanos : counter
    val env_get_file_size_nanos : counter
    val env_get_file_modification_time_nanos : counter
    val env_rename_file_nanos : counter
    val env_link_file_nanos : counter
    val env_lock_file_nanos : counter
    val env_unlock_file_nanos : counter
    val env_new_logger_nanos : counter
    val total_metric_count : counter

  end

  val create : unit -> perf_context


  val reset : perf_context -> unit
  val metric : perf_context -> counter -> int

end

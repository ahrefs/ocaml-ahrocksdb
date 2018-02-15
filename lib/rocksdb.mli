(** A binding to RocksDB

This library is a binding to Facebook's RocksDB library.
This library attempts to provide a layer on top of Rock's C FFI adapting it as
much as possible to OCaml idioms.
It aims to cover most of the C FFI in the long run, along with tests to ensure
no function is left never called and untested.

*)

(** High-level bindings for RocksDB open options

  This module provides a binding to the open options available in Rock's C FFIs.
  It provides a type [config] which holds the configuration options for opening a Rocksdb database
  that can be then turned into a proper [options] type that will be used to access
  a database.

*)
module Options : sig

  (** Opaque RocksDB configuration object, should be generated through {options_of_config}. Thread-safe access. *)
  type options

  module Tables : sig

    module Block_based : sig

      type t

      val create : block_size:int -> t

    end

  end

  type table_format = Block_based of Tables.Block_based.t

  type config = {
    parallelism_level : int option; (** Number of background processes used by RocksDB *)
    compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ]; (** Compression algorithm used to compact data *)
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
    table_format : table_format option;
  }

  (** default configuration, only compression is set to `Snappy, everything else is None (RocksDB defaults will apply) *)
  val default : config

  (** Applying a configuration and allocating an options type. *)
  val options_of_config : config -> options

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

(** Opaque database handle *)
type db

val open_db : ?create:bool -> options:Options.options -> name:string -> (db, string) result
(** [open_db create options name] will return an handle to the database in case
    of success or the error returned by RocksDB in case of failure.
    [create] allows to specify if the database must be created if it doesn't exists,
    returning an error in the event of an non-existing database being opened (default to false).
    [options] is {!Options.options} and must be created through {!Options.options_of_config}.
    [name] is the path to the database.
*)

val open_db_read_only : ?fail_on_wal:bool -> options:Options.options -> name:string -> (db, string) result
(** [open_db options name] will return a read-only handle to the database in case
    of success or the error returned by RocksDB in case of failure.
    [options] is {!Options.options} and must be created through {!Options.options_of_config}.
    [name] is the path to the database.
    [fail_on_wal] returns an error if write log is not empty
*)

val put : db -> Write_options.t -> key:string -> value:string -> (unit, string) result
(** [put db write_options key value] will write at key [key] the value [value], on database [db].
    Return unit on success, RocksDB reported error on error.
*)

val delete : db -> Write_options.t -> string -> (unit, string) result
(** [delete db write_options key] will delete key [key] on database [db].
    Return unit on success, RocksDB reported error on error.
*)

val get : db -> Read_options.t -> string -> [ `Error of string | `Not_found | `Ok of string ]
(** [get db read_options key] will fetch key [key] on database [db].
    Returns `Ok value if the key is found, `Not_found otherwise, and `Error if a failure occurred.
*)

val flush : db -> Flush_options.t -> (unit, string) result
(** [flush db flush_options] will flush all pending memtables on database [db].
    Return unit on success, RocksDB reported error on error.
*)

val compact_now : db -> (unit, string) result
(** [compact_now db] will initiate a compaction on all ranges available in database. This is an asynchronous operation, returning unit once operation is started. *)

val stats : db -> (string option, string) result
(** [stats db] will return the accumulated stats for this database handle as an optional string form *)

val close_db : db -> (unit, string) result
(** [close db] explicitly closes the db handle. Any further access will raise an error *)

(** Batch processing
    RocksDB allows to batch operations through a dedicated batch object that must be fed to {!write}.
    A batch object {!Batch.t} is a collection of operation to run on a database. (like {!Batch.put} or delete).
*)
module Batch : sig

  (** An opaque batch request must be created through {!create} and executed through {!write} *)
  type t

  (** [create] will create a batch job to be used to batch operation on the database. *)
  val create : unit -> t

  val count : t -> int

  val clear : t -> unit

  (** [put batch key value] will take a [batch] job and stage the writing of the [key] key and [value] value in the batch job. *)
  val put : t -> key:string -> value:string -> unit

  (** [write db write_options batch] takes a [db] handle, some [write_options] and a [batch] job and execute it on the database. *)
  val write : db -> Write_options.t -> t -> (unit, string) result

  (** A simple helper, will take a list of key_value and do a unique batch and write it to the database *)
  val simple_write_batch : db -> Write_options.t -> (string * string) list -> (unit, string) result

end

module Iterator : sig

  type t

  val create : db -> Read_options.t -> (t, string) result

  (** [seek iterator prefix] will set the iterator [t] in seek mode, iterating on keys starting by [prefix] *)
  val seek : t -> string -> unit

  (** [get iterator] will get the current key value pair on iterator [t]. Calling it multiple time in a row with no change of position results in the same pair being returned *)
  val get : t -> (string * string) option

  (** [next iterator] will set the iterator to the next key in the range. pair on iterator [t].
      Be mindful of the fact that you need to check if the iterator is still valid via {!is_valid}, and that according to RocksDB documentation, in prefix mode,
      you should make sure that the key is indeed starting by your prefix as your ending condition while iterating, since after finishing the range, RocksDB might return the next range after it.
      See https://github.com/facebook/rocksdb/wiki/Prefix-Seek-API-Changes#transition-to-the-new-usage
  *)
  val next : t -> unit

  val is_valid : t -> bool

end

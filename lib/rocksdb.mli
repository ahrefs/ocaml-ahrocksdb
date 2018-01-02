module Options : sig

  type options

  type config = {
    parallelism_level : int option;
    compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ];
  }

  val default : config
  val options_of_config : config -> options

end

type db

val open_db : ?create:bool -> options:Options.options -> name:string -> (db, string) result
val close_db : db -> unit

module Write_options : sig

  type t

  val create : ?disable_wal:int -> ?sync:bool -> unit -> t

end

module Read_options : sig

  type t

  val create : ?verify_checksums:bool -> ?fill_cache:bool -> ?tailing:bool -> unit -> t

end


val put : db -> Write_options.t -> key:string -> value:string -> (unit, string) result
val delete : db -> Write_options.t -> string -> (unit, string) result
val get : db -> Read_options.t -> string -> [ `Error of string | `Not_found | `Ok of string ]

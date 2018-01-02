module Options : sig

  type options

  type config = {
    parallelism_level : int option;
    compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ];
  }

  val default : config
  val create : config:config -> options

end

type db

val open_db : ?create:bool -> options:Options.options -> name:string -> (db, string) result
val close_db : db -> unit

type wopts
type ropts

val init_writeoptions : db -> wopts

val put : ?wopts:wopts -> db -> key:string -> value:string -> (unit, string) result
val delete : ?wopts:wopts -> db -> string -> (unit, string) result
val get : db -> string -> [ `Error of string | `Not_found | `Ok of string ]

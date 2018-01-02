module Options : sig

  type options

  type config = {
    parallelism_level : int option;
    compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ];
  }

  val default : config
  val create : config:config -> options

end

type t

val open_db : ?create:bool -> options:Options.options -> name:string -> (t, string) result
val close_db : t -> unit

type wopts
type ropts

val init_writeoptions : t -> wopts

val put : ?wopts:wopts -> t -> key:string -> value:string -> (unit, string) result
val delete : ?wopts:wopts -> t -> string -> (unit, string) result
val get : t -> string -> [ `Error of string | `Not_found | `Ok of string ]

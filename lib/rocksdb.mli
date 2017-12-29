module Options : sig

  type options

  type config = {
    parallelism_level : int option;
    compression : [ `Bz2 | `Lz4 | `Lz4hc | `No_compression | `Snappy | `Zlib ];
  }

  val default : config
  val create : config:config -> options

end

module Db : sig

  type t

  val open_db : ?create:bool -> options:Options.options -> name:string -> (t, string) result
  val close_db : t -> unit

end

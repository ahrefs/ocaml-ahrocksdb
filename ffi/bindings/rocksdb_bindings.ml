module M(F: Cstubs.FOREIGN) = struct

  let foreign = F.foreign

  module C = struct
    include Ctypes
    let (@->)         = F.(@->)
    let returning     = F.returning
  end

  type t = unit C.ptr

  module Options = struct

    type options = unit C.ptr
    let options : options C.typ = C.ptr C.void

    let create =
      foreign ("rocksdb_options_create") C.(void @-> returning options)

    let destroy =
      foreign ("rocksdb_options_destroy") C.(options @-> returning void)

    let increase_parallelism =
      foreign ("rocksdb_options_increase_parallelism") C.(options @-> int @-> returning void)

    let optimize_for_point_lookup =
      foreign ("rocksdb_options_optimize_for_point_lookup") C.(options @-> uint64_t @-> returning void)

    let optimize_level_style_compaction =
      foreign ("rocksdb_options_optimize_level_style_compaction") C.(options @-> uint64_t @-> returning void)

    let optimize_universal_style_compaction =
      foreign ("rocksdb_options_optimize_universal_style_compaction") C.(options @-> uint64_t @-> returning void)

    let compression_view =
      let read = function
        | 0 -> `No_compression
        | 1 -> `Snappy
        | 2 -> `Zlib
        | 3 -> `Bz2
        | 4 -> `Lz4
        | 5 -> `Lz4hc
        | other -> invalid_arg @@ Printf.sprintf "read_compression_view: invalid compression type: %d" other
      in
      let write = function
        | `No_compression -> 0
        | `Snappy -> 1
        | `Zlib -> 2
        | `Bz2 -> 3
        | `Lz4 -> 4
        | `Lz4hc -> 5
      in
      Ctypes.view ~read ~write Ctypes.int

    let set_compression =
      foreign "rocksdb_options_set_compression" C.(options @-> compression_view @-> returning void)

    let set_error_if_exists =
      foreign "rocksdb_options_set_error_if_exists" C.(options @-> Views.bool_to_uchar @-> returning void)

    let set_create_if_missing =
      foreign "rocksdb_options_set_create_if_missing" C.(options @-> Views.bool_to_uchar @-> returning void)

  end

  module Rocksdb = struct

    type t = unit C.ptr
    let t : t C.typ = C.ptr C.void

    let open_ =
      foreign "rocksdb_open" C.(Options.options @-> string @-> ptr string_opt @-> returning t)

    let close =
      foreign "rocksdb_close" C.(t @-> returning void)

  end

end

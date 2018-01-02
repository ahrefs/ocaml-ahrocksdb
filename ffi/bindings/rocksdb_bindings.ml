
module M(F: Cstubs.FOREIGN) = struct

  module V = Views
  open V

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
      foreign ("rocksdb_options_optimize_for_point_lookup") C.(options @-> int_to_uint64 @-> returning void)

    let optimize_level_style_compaction =
      foreign ("rocksdb_options_optimize_level_style_compaction") C.(options @-> int_to_uint64 @-> returning void)

    let optimize_universal_style_compaction =
      foreign ("rocksdb_options_optimize_universal_style_compaction") C.(options @-> int_to_uint64 @-> returning void)

    let set_compression =
      foreign "rocksdb_options_set_compression" C.(options @-> compression_view @-> returning void)

    let set_error_if_exists =
      foreign "rocksdb_options_set_error_if_exists" C.(options @-> Views.bool_to_uchar @-> returning void)

    let set_create_if_missing =
      foreign "rocksdb_options_set_create_if_missing" C.(options @-> Views.bool_to_uchar @-> returning void)

    let set_paranoid_checks =
      foreign "rocksdb_options_set_paranoid_checks" C.(options @-> Views.bool_to_uchar @-> returning void)

  end

  module Rocksdb = struct

    type t = unit C.ptr
    let t : t C.typ = C.ptr C.void

    let open_ =
      foreign "rocksdb_open" C.(Options.options @-> string @-> ptr string_opt @-> returning t)

    let close =
      foreign "rocksdb_close" C.(t @-> returning void)

    type wopts = unit C.ptr
    let wopts : wopts C.typ = C.ptr C.void

    let write_options_create =
      foreign "rocksdb_writeoptions_create" C.(t @-> returning wopts)

    let write_options_destroy =
      foreign "rocksdb_writeoptions_destroy" C.(wopts @-> returning void)

    let put =
      foreign "rocksdb_put"
        C.(t @-> wopts @-> ocaml_string @-> Views.int_to_size_t @-> ocaml_string @-> Views.int_to_size_t @-> ptr string_opt @-> returning void)
    let delete =
      foreign "rocksdb_delete"
        C.(t @-> wopts @-> ocaml_string @-> Views.int_to_size_t @-> ptr string_opt @-> returning void)

    type ropts = unit C.ptr
    let ropts : ropts C.typ = C.ptr C.void

    let read_options_create =
      foreign "rocksdb_readoptions_create" C.(t @-> returning ropts)

    let read_options_destroy =
      foreign "rocksdb_readoptions_destroy" C.(ropts @-> returning void)

    let get =
      foreign "rocksdb_get"
        C.(t @-> ropts @-> ocaml_string @-> Views.int_to_size_t @-> ptr Views.int_to_size_t @-> ptr string_opt @-> returning (ptr char))
    let free =
      foreign "rocksdb_free" C.(ptr void @-> returning void)
  end

end

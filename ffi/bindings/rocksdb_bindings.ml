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
      foreign "rocksdb_options_set_error_if_exists" C.(options @-> bool_to_uchar @-> returning void)

    let set_create_if_missing =
      foreign "rocksdb_options_set_create_if_missing" C.(options @-> bool_to_uchar @-> returning void)

    let set_paranoid_checks =
      foreign "rocksdb_options_set_paranoid_checks" C.(options @-> bool_to_uchar @-> returning void)

    let set_max_background_flushes =
      foreign ("rocksdb_options_set_max_background_flushes") C.(options @-> bool_to_int @-> returning void)

    let set_disable_auto_compactions =
      foreign "rocksdb_options_set_disable_auto_compactions" C.(options @-> bool_to_int @-> returning void)

    let set_level0_file_num_compaction_trigger =
      foreign "rocksdb_options_set_level0_file_num_compaction_trigger" C.(options @-> int_to_size_t @-> returning void)

    let set_level0_slowdown_writes_trigger =
      foreign "rocksdb_options_set_level0_slowdown_writes_trigger" C.(options @-> int_to_size_t @-> returning void)

    let set_level0_stop_writes_trigger =
      foreign "rocksdb_options_set_level0_stop_writes_trigger" C.(options @-> int_to_size_t @-> returning void)

  end

  module Rocksdb = struct

    type db = unit C.ptr
    let db : db C.typ = C.ptr C.void

    let open_ =
      foreign "rocksdb_open" C.(Options.options @-> string @-> ptr string_opt @-> returning db)

    let close =
      foreign "rocksdb_close" C.(db @-> returning void)

    module Write_options = struct

      type t = unit C.ptr
      let t : t C.typ = C.ptr C.void

      let create =
        foreign "rocksdb_writeoptions_create" C.(void @-> returning t)

      let destroy =
        foreign "rocksdb_writeoptions_destroy" C.(t @-> returning void)

      let set_sync =
        foreign "rocksdb_writeoptions_set_sync" C.(t @-> bool_to_uchar @-> returning void)

      (* not in rocksdb-4.5fb: disabled  *)

      (* let set_ignore_missing_column_families = *)
      (*   foreign "rocksdb_writeoptions_set_ignore_missing_column_families" C.(t @-> Views.bool_to_uchar @-> returning void) *)

      (* let set_no_slowdown = *)
      (*   foreign "rocksdb_writeoptions_set_no_slowdown" C.(t @-> Views.bool_to_uchar @-> returning void) *)

      (* let set_low_pri = *)
      (*   foreign "rocksdb_writeoptions_set_low_pri" C.(t @-> Views.bool_to_uchar @-> returning void) *)

      let disable_WAL =
        foreign "rocksdb_writeoptions_disable_WAL" C.(t @-> int @-> returning void)

    end

    module Read_options = struct

      type t = unit C.ptr
      let t : t C.typ = C.ptr C.void

      let create =
        foreign "rocksdb_readoptions_create" C.(void @-> returning t)

      let destroy =
        foreign "rocksdb_readoptions_destroy" C.(t @-> returning void)

      let set_verify_checksums =
        foreign "rocksdb_readoptions_set_verify_checksums" C.(t @-> bool_to_uchar @-> returning void)

      let set_fill_cache =
        foreign "rocksdb_readoptions_set_fill_cache" C.(t @-> bool_to_uchar @-> returning void)

      let set_tailing =
        foreign "rocksdb_readoptions_set_tailing" C.(t @-> bool_to_uchar @-> returning void)

    end

    module Batch = struct

      type t = unit C.ptr
      let t : t C.typ = C.ptr C.void

      let create =
        foreign "rocksdb_writebatch_create" C.(void @-> returning t)

      let destroy =
        foreign "rocksdb_writebatch_destroy" C.(t @-> returning void)

      let clear =
        foreign "rocksdb_writebatch_clear" C.(t @-> returning void)

      let count =
        foreign "rocksdb_writebatch_count" C.(t @-> returning int)

      let put =
        foreign "rocksdb_writebatch_put"
          C.(t @-> ocaml_string @-> int_to_size_t @-> ocaml_string @-> int_to_size_t @-> returning void)

    end

    let put =
      foreign "rocksdb_put"
        C.(db @-> Write_options.t @-> ocaml_string @-> int_to_size_t @-> ocaml_string @-> int_to_size_t @-> ptr string_opt @-> returning void)

    let delete =
      foreign "rocksdb_delete"
        C.(db @-> Write_options.t @-> ocaml_string @-> int_to_size_t @-> ptr string_opt @-> returning void)

    let get =
      foreign "rocksdb_get"
        C.(db @-> Read_options.t @-> ocaml_string @-> int_to_size_t @-> ptr int_to_size_t @-> ptr string_opt @-> returning (ptr char))

    let write =
      foreign "rocksdb_write"
        C.(db @-> Write_options.t @-> Batch.t @-> ptr string_opt @-> returning void)

    let free =
      foreign "rocksdb_free" C.(ptr void @-> returning void)


  end

end

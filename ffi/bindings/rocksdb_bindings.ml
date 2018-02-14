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

    module Tables = struct

      module BlockBased = struct

        type t = unit C.ptr
        let t : t C.typ = C.ptr C.void

        let create =
          foreign ("rocksdb_block_based_options_create") C.(void @-> returning t)

        let destroy =
          foreign ("rocksdb_block_based_options_destroy") C.(t @-> returning void)

        let set_block_size =
          foreign ("rocksdb_block_based_options_set_block_size") C.(t @-> int_to_size_t @-> returning void)

      end

    end

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
      foreign ("rocksdb_options_set_max_background_flushes") C.(options @-> int @-> returning void)

    let set_disable_auto_compactions =
      foreign "rocksdb_options_set_disable_auto_compactions" C.(options @-> bool_to_int @-> returning void)

    let set_level0_file_num_compaction_trigger =
      foreign "rocksdb_options_set_level0_file_num_compaction_trigger" C.(options @-> int_to_size_t @-> returning void)

    let set_level0_slowdown_writes_trigger =
      foreign "rocksdb_options_set_level0_slowdown_writes_trigger" C.(options @-> int_to_size_t @-> returning void)

    let set_level0_stop_writes_trigger =
      foreign "rocksdb_options_set_level0_stop_writes_trigger" C.(options @-> int_to_size_t @-> returning void)

    let set_write_buffer_size =
      foreign "rocksdb_options_set_write_buffer_size" C.(options @-> int_to_size_t @-> returning void)

    let set_max_write_buffer_number =
      foreign "rocksdb_options_set_max_write_buffer_number" C.(options @-> int @-> returning void)

    let set_min_write_buffer_number_to_merge =
      foreign "rocksdb_options_set_min_write_buffer_number_to_merge" C.(options @-> int @-> returning void)

    let set_memtable_vector_rep =
      foreign "rocksdb_options_set_memtable_vector_rep" C.(options @-> returning void)

    let prepare_for_bulk_load =
      foreign "rocksdb_options_prepare_for_bulk_load" C.(options @-> returning void)

    let set_target_file_size_base =
      foreign "rocksdb_options_set_target_file_size_base" C.(options @-> int_to_uint64 @-> returning void)

    let set_target_file_size_multiplier =
      foreign "rocksdb_options_set_target_file_size_multiplier" C.(options @-> int_to_uint64 @-> returning void)

    let set_num_levels =
      foreign "rocksdb_options_set_num_levels" C.(options @-> int @-> returning void)

    let set_block_based_table_factory =
      foreign "rocksdb_options_set_block_based_table_factory" C.(options @-> Tables.BlockBased.t @-> returning void)

  end

  module Rocksdb = struct

    type db = unit C.ptr
    let db : db C.typ = C.ptr C.void

    let open_ =
      foreign "rocksdb_open" C.(Options.options @-> string @-> ptr string_opt @-> returning db)

    let open_read_only =
      foreign "rocksdb_open_for_read_only" C.(Options.options @-> string @-> bool_to_uchar @-> ptr string_opt @-> returning db)

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
        foreign "rocksdb_writeoptions_disable_WAL" C.(t @-> Views.bool_to_int @-> returning void)

    end

    module Flush_options = struct

      type t = unit C.ptr
      let t : t C.typ = C.ptr C.void

      let create =
        foreign "rocksdb_flushoptions_create" C.(void @-> returning t)

      let destroy =
        foreign "rocksdb_flushoptions_destroy" C.(t @-> returning void)

      let wait =
        foreign "rocksdb_flushoptions_set_wait" C.(t @-> Views.bool_to_int @-> returning void)
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

    module Iterator = struct

      type t = unit C.ptr
      let t : t C.typ = C.ptr C.void

      let create =
        foreign "rocksdb_create_iterator" C.(db @-> Read_options.t @-> returning t)

      let destroy =
        foreign "rocksdb_iter_destroy" C.(t @-> returning void)

      let valid =
        foreign "rocksdb_iter_valid" C.(t @-> returning bool_to_uchar)

      let seek_to_first =
        foreign "rocksdb_iter_seek_to_first" C.(t @-> returning void)

      let seek_to_last =
        foreign "rocksdb_iter_seek_to_last" C.(t @-> returning void)

      let seek =
        foreign "rocksdb_iter_seek" C.(t @-> ocaml_string @-> Views.int_to_size_t @-> returning void)

      let next =
        foreign "rocksdb_iter_next" C.(t @-> returning void)

      let prev =
        foreign "rocksdb_iter_prev" C.(t @-> returning void)

      let key =
        foreign "rocksdb_iter_key" C.(t @-> ptr int_to_size_t @-> returning (ptr char))

      let value =
        foreign "rocksdb_iter_value" C.(t @-> ptr int_to_size_t @-> returning (ptr char))

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

    let flush =
      foreign "rocksdb_flush"
        C.(db @-> Flush_options.t @-> ptr string_opt @-> returning void)

    let compact_range =
      foreign "rocksdb_compact_range"
        C.(db @-> ptr_opt char @-> int @-> ptr_opt char @-> int @-> returning void)

    let property_value =
      foreign
        "rocksdb_property_value"
        C.(db @-> string @-> returning (ptr_opt char))

    let free =
      foreign "rocksdb_free" C.(ptr void @-> returning void)

  end

end

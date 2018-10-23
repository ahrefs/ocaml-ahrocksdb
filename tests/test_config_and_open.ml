open Rresult.R.Infix
open Rocksdb

let simple_open_default () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    (* >>= fun _ -> open_db ~create:false ~options ~name *)
    >>= fun _ -> Ok () (* FIXME *)
  end

let open_not_random_setters () =
  Utils.with_tmp_dir begin fun name ->
    let filter_policy = Options.Filter_policy.create_bloom_full ~bits_per_key:12 in
    let block_based_table = Options.Tables.Block_based.create ~block_size:(64 * 1024 *1024) in
    Options.Tables.Block_based.set_filter_policy block_based_table filter_policy;
    let config = {
      Options.default with
      Options.compression = `No_compression;
      max_flush_processes = Some 2;
      compaction_trigger = Some 128;
      slowdown_writes_trigger = Some 128;
      stop_writes_trigger = Some 256;
      disable_compaction = false;
      parallelism_level = Some 4;
      memtable_representation = None;
      num_levels = Some 1;
      target_base_file_size = None;
      table_format = Some (Block_based block_based_table);
      max_open_files = Some (-1);
    }
    in
    let options = Options.options_of_config config in
    open_db ~create:true ~options ~name
    >>= fun _ -> Ok ()
  end

let open_error () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    match open_db ~create:false ~options ~name with
    | Error _ -> Ok ()
    | Ok _ ->
      Error "Test_config_and_open.open error failed: open was successful"
  end

let tests = [
  "simple_open_default", simple_open_default;
  "open_not_random_setters", open_not_random_setters;
  "open_error", open_error;
]

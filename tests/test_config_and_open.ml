open Rresult.R.Infix
open Rocksdb

let simple_open_default () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    >>| close_db
    >>= fun () -> open_db ~create:false ~options ~name
    >>| close_db
  end

let open_not_random_setters () =
  Utils.with_tmp_dir begin fun name ->
    let config = {
      Options.compression = `Snappy;
      max_flush_processes = Some 2;
      compaction_trigger = Some 128;
      slowdown_writes_trigger = Some 128;
      stop_writes_trigger = Some 256;
      disable_compaction = false;
      parallelism_level = Some 4;
      memtable_representation = None;
    }
    in
    let options = Options.options_of_config config in
    open_db ~create:true ~options ~name
    >>| close_db

  end

let open_error () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    match open_db ~create:false ~options ~name with
    | Error _ -> Ok ()
    | Ok db ->
      close_db db;
      Error "Test_config_and_open.open error failed: open was successful"
  end

let tests = [
  "simple_open_default", simple_open_default;
  "open_not_random_setters", open_not_random_setters;
  "open_error", open_error;
]

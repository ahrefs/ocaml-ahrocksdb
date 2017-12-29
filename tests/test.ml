let test_default_options =
  let open Rocksdb in
  let config = Options.default in
  let _ = Options.create ~config in
  ()

let test_all_config_setters =
  let open Rocksdb_ffi.M in
  let options = Options.create () in
  Options.increase_parallelism options 4;
  Options.optimize_for_point_lookup options Unsigned.UInt64.one;
  Options.optimize_level_style_compaction options Unsigned.UInt64.one;
  Options.optimize_universal_style_compaction options Unsigned.UInt64.one;
  Options.destroy options;
  ()

let test_open_err =
  let open Rocksdb in
  let config = Options.default in
  let options = Options.create ~config in
  match Db.open_db ~create:false ~options ~name:"/tmp/ocaml-rocksdb-should-not-exists" with
  | Ok handle -> Db.close_db handle; failwith "handle was opened"
  | Error err -> ()

let test_open =
  let open Rocksdb in
  let config = Options.default in
  let options = Options.create ~config in
  match Db.open_db ~create:true ~options ~name:"/tmp/rocks_test" with
  | Ok handle -> Db.close_db handle
  | Error err -> failwith err

let test_options =
  [
    "Testing default options", `Quick, (fun () -> Alcotest.(check unit) "default options" () test_default_options);
    "Testing options setters bindings", `Quick, (fun () -> Alcotest.(check unit) "all setters" () test_all_config_setters);
  ]

let test_open =
  [
    "Testing simple handle opening with path error", `Quick, (fun () -> Alcotest.(check unit) "open err" () test_open_err);
    "Testing simple handle opening", `Quick, (fun () -> Alcotest.(check unit) "open" () test_open);
  ]

let () =
  Alcotest.run "ocaml-rocksdb" [
    "Test default options", test_options;
    "Test open database", test_open
  ]


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

let test_options =
  [
    "Teesting default options", `Quick, (fun () -> Alcotest.(check unit) "default options" () test_default_options);
    "Testing options setters bindings", `Quick, (fun () -> Alcotest.(check unit) "all setters" () test_all_config_setters);
  ]

let () =
  Alcotest.run "tests" [
    "Test default options", test_options
  ]

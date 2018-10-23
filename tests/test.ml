type error = string
let pp_error ppf x = Fmt.string ppf x
let error = Alcotest.testable pp_error (=)
let t = Alcotest.(result unit error)

let check (name, fn) = name, `Quick, (fun () -> Alcotest.check t name (Ok ()) (fn ()))

let config_and_open_tests = List.map check Test_config_and_open.tests
let writes_tests = List.map check Test_writes.tests
let iterator_tests = List.map check Test_iterator.tests

let () =
  Alcotest.run "RocksDB binding" [
    "config_and_open_tests", config_and_open_tests;
    "writes_tests", writes_tests;
    "iterator_tests", iterator_tests
  ]
(* let test_default_options = *)
(*   let open Rocksdb in *)
(*   let config = Options.default in *)
(*   let _ = Options.create ~config in *)
(*   () *)

(* let test_all_config_setters = *)
(*   let open Rocksdb_ffi.M in *)
(*   let options = Options.create () in *)
(*   Options.increase_parallelism options 4; *)
(*   Options.optimize_for_point_lookup options 1; *)
(*   Options.optimize_level_style_compaction options 1; *)
(*   Options.optimize_universal_style_compaction options 1; *)
(*   Options.destroy options; *)
(*   () *)

(* let test_open_err = *)
(*   let open Rocksdb in *)
(*   let config = Options.default in *)
(*   let options = Options.create ~config in *)
(*   match open_db ~create:false ~options ~name:"/tmp/ocaml-rocksdb-should-not-exists" with *)
(*   | Ok handle -> close_db handle; failwith "handle was opened" *)
(*   | Error err -> () *)

(* let test_open = *)
(*   let open Rocksdb in *)
(*   let config = Options.default in *)
(*   let options = Options.create ~config in *)
(*   match open_db ~create:true ~options ~name:"/tmp/rocks_test" with *)
(*   | Ok handle -> close_db handle *)
(*   | Error err -> failwith err *)

(* let test_wrd = *)
(*   let test _ = *)
(*     let open Rocksdb in *)
(*     let config = Options.default in *)
(*     let options = Options.create ~config in *)
(*     open_db ~create:true ~options ~name:"/tmp/rocksrocks" *)
(*     >>= fun handle -> *)
(*     let key = "llama" in *)
(*     let value = "fluffy" in *)
(*     put handle ~key ~value *)
(*     >>= fun () -> begin *)
(*     match get handle key with *)
(*     | `Ok value_stored -> if value = value_stored then Ok () else failwith "uh" *)
(*     | _ -> failwith "lol" end *)
(*     >>= fun () -> *)
(*     delete handle key >>= fun () -> begin *)
(*     match get handle key with *)
(*     | `Error _ *)
(*     | `Ok _ -> failwith "uuh" *)
(*     | `Not_found -> Ok () *)
(*   end *)
(*   in *)
(*   match test () with *)
(*   | Ok () -> () *)
(*   | _ -> failwith "fail" *)

(* let test_options = *)
(*   [ *)
(*     "Testing default options", `Quick, (fun () -> Alcotest.(check unit) "default options" () test_default_options); *)
(*     "Testing options setters bindings", `Quick, (fun () -> Alcotest.(check unit) "all setters" () test_all_config_setters); *)
(*   ] *)

(* let test_open = *)
(*   [ *)
(*     "Testing simple handle opening with path error", `Quick, (fun () -> Alcotest.(check unit) "open err" () test_open_err); *)
(*     "Testing simple handle opening", `Quick, (fun () -> Alcotest.(check unit) "open" () test_open); *)
(*   ] *)

(* let test_wrd = *)
(*   [ *)
(*     "Testing simple write/read/delete scenario", `Quick, (fun () -> Alcotest.(check unit) "wrd test" () test_wrd); *)
(*   ] *)

(* let () = *)
(*   Alcotest.run "ocaml-rocksdb" [ *)
(*     "Test default options", test_options; *)
(*     "Test open database", test_open; *)
(*     "Test simple write read delete", test_wrd; *)
(*   ] *)

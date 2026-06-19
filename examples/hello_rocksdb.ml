let () =
  let config = Rocksdb.Options.{ default with create_if_missing = true } in
  let db_path = "/tmp/hello_rocksdb" in
  let db = match Rocksdb.open_db ~config ~name:db_path with
    | Ok db -> db
    | Error (`Msg msg) -> Printf.eprintf "open_db: %s\n" msg; exit 1
  in
  let wo = Rocksdb.Options.Write_options.create () in
  let ro = Rocksdb.Options.Read_options.create () in
  (match Rocksdb.put db wo ~key:"hello" ~value:"world" with
   | Ok () -> ()
   | Error (`Msg msg) -> Printf.eprintf "put: %s\n" msg; exit 1);
  (match Rocksdb.get db ro "hello" with
   | Ok (`Found v) -> Printf.printf "hello -> %s\n%!" v
   | Ok `Not_found  -> print_endline "not found"
   | Error (`Msg msg) -> Printf.eprintf "get: %s\n" msg; exit 1);
  match Rocksdb.close_db db with
  | Ok () -> ()
  | Error (`Msg msg) -> Printf.eprintf "close_db: %s\n" msg; exit 1

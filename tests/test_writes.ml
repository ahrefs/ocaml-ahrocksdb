open Rresult.R.Infix
open Rocksdb
open Printf

let write_one () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    >>= fun db ->
    let write_options = Write_options.create () in
    let key = "cyber" in
    let value = "llama" in
    put db write_options ~key ~value
    >>= fun () ->
    let read_options = Read_options.create () in
    match get db read_options key with
    | `Ok value' -> if String.equal value value' then Ok () else Error (sprintf "Wrong value retrieved: %s expected %s" value' value)
    | `Not_found -> Error (sprintf "key %s not found" key)
    | `Error err -> Error err
  end

let write_one_ttl () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db_with_ttl ~create:true ~options ~name ~ttl:1
    >>= fun db ->
    let write_options = Write_options.create () in
    let key = "cyber" in
    let value = "llama" in
    put db write_options ~key ~value
    >>= fun () ->
    Unix.sleep 3;
    Rocksdb.compact_now db >>= fun () ->
    let read_options = Read_options.create () in
    match get db read_options key with
    | `Ok _ -> Error ("Key was not removed by compaction in TTL mode")
    | `Not_found -> Ok ()
    | `Error err -> Error err
  end
let update_one () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    >>= fun db ->
    let write_options = Write_options.create () in
    let key = "cyber" in
    let value = "llama" in
    put db write_options ~key ~value
    >>= fun () ->
    let value2 = "llama2" in
    put db write_options ~key ~value:value2
    >>= fun () ->
    let read_options = Read_options.create () in
    match get db read_options key with
    | `Ok value' -> if String.equal value2 value' then Ok () else Error (sprintf "Wrong value retrieved: %s expected %s" value' value2)
    | `Not_found -> Error (sprintf "key %s not found" key)
    | `Error err -> Error err
  end

let delete_one () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    >>= fun db ->
    let write_options = Write_options.create () in
    let key = "cyber" in
    let value = "llama" in
    put db write_options ~key ~value
    >>= fun () ->
    delete db write_options key
    >>= fun () ->
    let read_options = Read_options.create () in
    match get db read_options key with
    | `Ok _ -> Error "delete_one"
    | `Not_found -> Ok ()
    | `Error err -> Error err
  end

let write_one_err () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    >>= fun db ->
    let write_options = Write_options.create () in
    let key = "cyber" in
    let value = "llama" in
    put db write_options ~key ~value
    >>= fun () ->
    let read_options = Read_options.create () in
    match get db read_options "bad key" with
    | `Ok _ -> Error "write_one_err"
    | `Not_found -> Ok ()
    | `Error err -> Error err
  end

let write_batch_many () =
  let kvs = Utils.get_random_kvalues 10_000 in
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    >>= fun db ->
    let write_options = Write_options.create () in
    Batch.simple_write_batch db write_options kvs;
    >>= fun () ->
    let read_options = Read_options.create () in
    List.fold_left begin fun r (key, value) ->
      r >>= fun () ->
      match get db read_options key with
      | `Ok value' -> if String.equal value value' then Ok () else Error "write_many: bad value"
      | `Not_found -> Error "write_many"
      | `Error err -> Error err
    end (Ok ()) kvs
  end

let write_many () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    >>= fun db ->
    let write_options = Write_options.create () in
    let kvs = Utils.get_random_kvalues 10_000 in
    List.fold_left begin fun r (key, value) ->
      r >>= fun () ->
      put db write_options ~key ~value
    end (Ok ()) kvs
    >>= fun () ->
    let read_options = Read_options.create () in
    List.fold_left begin fun r (key, value) ->
      r >>= fun () ->
      match get db read_options key with
      | `Ok value' -> if String.equal value value' then Ok () else Error "write_many: bad value"
      | `Not_found -> Error "write_many"
      | `Error err -> Error err
    end (Ok ()) kvs
  end

let tests = [
  "write_one", write_one;
  "write_one_ttl", write_one_ttl;
  "write_one_err", write_one_err;
  "write_many", write_many;
  "write_batch_many", write_batch_many;
  "delete_one", delete_one;
  "update_one", update_one;
]

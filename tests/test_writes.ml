open Rresult.R.Infix
open Rocksdb
open Bos
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

let get_random_kvalues n =
  let rng = Cryptokit.Random.device_rng "/dev/urandom" in
  if n < 0 then failwith "get_random_kvalues";
  let rec aux acc = function
    | 0 -> acc
    | n ->
      let key = Cryptokit.Random.string rng 32 in
      let value = Cryptokit.Random.string rng 32 in
      aux ((key, value)::acc) (n - 1)
  in
  aux [] n


let write_many () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    >>= fun db ->
    let write_options = Write_options.create () in
    let kvs = get_random_kvalues 10_000 in
    List.fold_left begin fun r (key, value) ->
      r >>= fun () ->
      put db write_options ~key ~value
    end (Ok ()) kvs
    >>= fun () ->
    let read_options = Read_options.create () in
    List.fold_left begin fun r (key, value) ->
      r >>= fun () ->
      match get db read_options key with
      | `Ok value' -> if String.equal value value' then Ok () else Error (sprintf "Wrong value retrieved: %s expected %s" value' value)
      | `Not_found -> Error (sprintf "key %s not found" key)
      | `Error err -> Error err
    end (Ok ()) kvs
  end

let tests = [
  "write_one", write_one;
  "write_one_err", write_one_err;
  "write_many", write_many;
]

open Rocksdb
open Rresult.R.Infix
open Printf

let simple_iterator_test () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    >>= fun db ->
    let write_options = Write_options.create () in
    let kvs = Utils.get_random_kvalues 1000 in
    List.fold_left begin fun r (key, value) ->
      r >>= fun () ->
      let key = "prefix" ^ key in
      put db write_options ~key ~value
    end (Ok ()) kvs
    >>= fun () ->
    let read_options = Read_options.create () in
    Iterator.create db read_options >>= fun it ->
    Iterator.seek it "prefix";
    let rec walk acc =
      match Iterator.get it with
      | Some (key, _) when Astring.String.is_prefix ~affix:"prefix" key -> Iterator.next it; walk (acc + 1)
      | _ -> acc
    in
    let res = walk 0 in
    if res = 1000 then Ok () else Error (sprintf "simple_iterator_test: got %d keys" res)
  end

let simple_iterator_test_two_prefixes () =
  Utils.with_tmp_dir begin fun name ->
    let options = Options.options_of_config Options.default in
    open_db ~create:true ~options ~name
    >>= fun db ->
    let write_options = Write_options.create () in
    let kvs = Utils.get_random_kvalues 1000 in
    List.fold_left begin fun r (key, value) ->
      r >>= fun () ->
      let key = "prefix" ^ key in
      let key2 = "pprefix" ^ key in
      put db write_options ~key ~value
      >>= fun _ -> put db write_options ~key:key2 ~value
    end (Ok ()) kvs
    >>= fun () ->
    let read_options = Read_options.create () in
    Iterator.create db read_options >>= fun it ->
    Iterator.seek it "prefix";
    let rec walk acc =
      match Iterator.get it with
      | Some (key, _) when Astring.String.is_prefix ~affix:"prefix" key -> Iterator.next it; walk (acc + 1)
      | _ -> acc
    in
    let res = walk 0 in
    if res = 1000 then Ok () else Error (sprintf "simple_iterator_test_two_prefixes: got %d keys" res)
  end

let tests = [
  "simple_iterator_test", simple_iterator_test;
  "simple_iterator_test_two_prefixes", simple_iterator_test_two_prefixes;
]

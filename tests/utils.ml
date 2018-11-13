open Bos

let with_tmp_dir fn =
  let result =
    OS.Dir.with_tmp "%s" begin fun path () ->
      let name = Fpath.to_string path in
      fn name
    end ()
  in
  match result with
  | Ok result -> result
  | Error _ -> Error (`Msg "Bos err: OS.Dir.with_tmp")

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

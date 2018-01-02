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
  | Error _ -> Error "Bos err: OS.Dir.with_tmp"

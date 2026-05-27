open Printf

let minimum_rocks_major, minimum_rocks_minor = 5, 14

module C = Configurator.V1


let pkg_config_raw ?(args=[]) ~package c =
match C.Process.run c "pkg-config" (args @ [ package ]) with
| { exit_code = 0; stdout; _ } -> Ok stdout
| { stderr; _ } -> Error stderr
let input_all_lines ic =
  let lines = ref [] in
  let rec loop () =
    try
      lines := input_line ic :: !lines;
      loop ()
    with End_of_file ->
      List.rev !lines
  in
  loop ()

let () = C.main ~name:"librocksdb" begin fun c ->
let std_lib_dirs = [
  "/usr/lib"; "/usr/lib/x86_64-linux-gnu"; "/usr/lib/aarch64-linux-gnu";
  "/usr/local/lib"; "/usr/lib64";
] in
let has_static_lib name =
  List.exists (fun dir ->
    Sys.file_exists (Filename.concat dir ("lib" ^ name ^ ".a"))
  ) std_lib_dirs
in
let libs_private =
  List.filter_map (fun name ->
    if has_static_lib name then Some ("-l" ^ name) else None
  ) ["z"; "snappy"; "lz4"; "bz2"; "zstd"]
  @ ["-ldl"; "-lpthread"; "-lstdc++"]
in
let pc_dir = Sys.getcwd () in
let pc_orig_path = match pkg_config_raw ~args:["--path"] ~package:"rocksdb" c with
| Ok v -> v
| Error _ -> C.die "cant get path of .pc file" in
let ic = open_in (String.trim pc_orig_path) in
let pc_orig = input_all_lines ic in
close_in ic;
let oc = open_out (Filename.concat pc_dir "rocksdb.pc") in
List.iter (fprintf oc "%s\n") pc_orig;
fprintf oc "Libs.private: %s\n" (String.concat " " libs_private);
close_out oc;
let existing = match Sys.getenv_opt "PKG_CONFIG_PATH" with
  | Some s -> ":" ^ s
  | None -> ""
in
let () = Unix.putenv "PKG_CONFIG_PATH" (pc_dir ^ existing) in
let () = Unix.putenv "PKG_CONFIG_ARGN" "--static" in
let c_flags, link_flags =
  (* need system cflags otherwise `<rocks/c.h>` can't be `#include`d *)
  Unix.putenv "PKG_CONFIG_ALLOW_SYSTEM_CFLAGS" "1";
  match C.Pkg_config.get c with
  | None ->
    eprintf "discover requires pkg-config\n";
    C.die "discover error"
  | Some pc ->
      let expr = sprintf "rocksdb >= %d.%d" minimum_rocks_major minimum_rocks_minor in
      match C.Pkg_config.query_expr_err pc ~package:"rocksdb" ~expr with
      | Error s ->
          eprintf "pkg-config query error: %s\n" s;
          C.die "discover error"
      | Ok { C.Pkg_config.cflags; libs} -> cflags, libs
in

C.Flags.write_sexp  "c_flags.sexp"          c_flags;
C.Flags.write_sexp  "c_library_flags.sexp"  link_flags;
C.Flags.write_lines "c_flags.txt"           c_flags;
C.Flags.write_lines "c_library_flags.txt"   link_flags;

end

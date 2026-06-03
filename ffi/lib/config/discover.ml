open Configurator.V1

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
let pkg_config_raw c ~args ~package =
  let args = args @ [ package ] in
  match Process.run c "pkg-config" args with
  | { exit_code = 0; stdout; _ } -> Ok stdout
  | { stderr; _ } -> Error stderr
let std_lib_dirs = [
  "/usr/lib"; "/usr/lib/x86_64-linux-gnu"; "/usr/lib/aarch64-linux-gnu";
  "/usr/local/lib"; "/usr/lib64";
]

let () = main ~name:"librocksdb" begin fun c ->

(* Part1: tweak librocksdb's incomplete pkg-config file *)
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
let pc_orig_path =
  match pkg_config_raw c ~args:["--path"] ~package:"rocksdb" with
  | Error s -> die "cannot find rocksdb pkg-config file: %S" s
  | Ok path -> path
in
let pc_orig =
  let ic = open_in (String.trim pc_orig_path) in
  let r = input_all_lines ic in
  close_in ic;
  r
in
let localdir = Sys.getcwd () in
let () =
  let oc = open_out (Filename.concat localdir "rocksdb.pc") in
  List.iter (Printf.fprintf oc "%s\n") pc_orig;
  Printf.fprintf oc "Libs.private: %s\n" (String.concat " " libs_private);
  close_out oc
in
(* patch PKG_CONFIG_PATH to allow discovery of patched .pc *)
let () = match Sys.getenv_opt "PKG_CONFIG_PATH" with
  | Some s -> Unix.putenv "PKG_CONFIG_PATH" (localdir ^ ":" ^ s)
  | None -> Unix.putenv "PKG_CONFIG_PATH" localdir
in

(* Part2: actual configure *)
  Unix.putenv "PKG_CONFIG_ARGN" "--static";
  let c_flags, link_flags =
    match Pkg_config.get c with
    | None -> die "discover requires pkg-config"
    | Some pc ->
    match Pkg_config.query_expr_err pc ~package:"rocksdb" ~expr:"rocksdb >= 5.14" with
      | Error s -> die "pkg-config query error: %s" s;
      | Ok { Pkg_config.cflags; libs} -> cflags, libs
  in
  Flags.write_sexp  "c_flags.sexp"          c_flags;
  Flags.write_sexp  "c_library_flags.sexp"  link_flags;
  Flags.write_lines "c_flags.txt"           c_flags;
  Flags.write_lines "c_library_flags.txt"   link_flags;
end

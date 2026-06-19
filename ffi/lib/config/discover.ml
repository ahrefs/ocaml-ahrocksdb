open Configurator.V1

let pkg_config_raw ?(args=[]) ~package c =
match Process.run c "pkg-config" (args @ [ package ]) with
| { exit_code = 0; stdout; _ } -> String.trim stdout
| { stderr; _ } -> die "cannot run pkg-config: %s\n" stderr

let () = main ~name:"librocksdb" begin fun c ->

(* upstream pc files are inaccurate, debian inherits this inaccuracy, we fix it *)
let system_info = pkg_config_raw ~args:["--libs"; "--static"] ~package:"rocksdb" c in
let () =
  match system_info with
  | "-lrocksdb" -> begin
    (* innacurate pc file detected *)
    let cwd = Sys.getcwd () in
    let () = match Process.run c "cp" [pkg_config_raw ~args:["--path"] ~package:"rocksdb" c; Filename.concat cwd "rocksdb.pc"] with
      | { exit_code = 0; _ } -> ()
      | _ -> die "cannot cp pc file"
    in
    let libs_private =
      let std_lib_dirs = [ "/usr/lib"; "/usr/lib/x86_64-linux-gnu"; "/usr/lib/aarch64-linux-gnu"; "/usr/local/lib"; "/usr/lib64"; ] in
      let has_static_lib name =
        List.exists
          (fun dir -> Sys.file_exists (Filename.concat dir ("lib" ^ name ^ ".a")))
          std_lib_dirs
      in
      List.filter_map (fun name -> if has_static_lib name then Some ("-l" ^ name) else None) ["z"; "snappy"; "lz4"; "bz2"; "zstd"]
      @ ["-ldl"; "-lpthread"; "-lstdc++"]
    in
    let () = match Process.run c "sh" ["-c"; Printf.sprintf "echo 'Libs.private: %s' >> ./rocksdb.pc" (String.concat " " libs_private);] with
      | { exit_code = 0; _ } -> ()
      | _ -> die "cannot append to pc file"
    in
    let existing = match Sys.getenv_opt "PKG_CONFIG_PATH" with | Some s -> ":" ^ s | None -> "" in
    let () = Unix.putenv "PKG_CONFIG_PATH" (cwd ^ existing) in
    ()
  end
  | _ -> ()
in


  let c_flags, link_flags =
    let () = Unix.putenv "PKG_CONFIG_ARGN" "--static" in
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

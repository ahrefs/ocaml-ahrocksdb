open Configurator.V1

let () = main ~name:"librocksdb" begin fun c ->
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

open Printf

let minimum_rocks_major, minimum_rocks_minor = 5, 14

module C = Configurator.V1

let () = C.main ~name:"librocksdb" begin fun c ->

let c_flags, link_flags =
  match C.Pkg_config.get c with
  | None ->
    eprintf "discover requires pkg-config\n";
    C.die "discover error"
  | Some pc ->
      (* need system cflags otherwise `<rocks/c.h>` can't be `#include`d *)
      Unix.putenv "PKG_CONFIG_ALLOW_SYSTEM_CFLAGS" "1";
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

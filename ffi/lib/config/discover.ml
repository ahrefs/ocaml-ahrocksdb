open Printf
open Base

module C = Configurator.V1

let () = C.main ~name:"librocksdb" begin fun c ->

let link_flags = ["-lrocksdb"] in

let known_includes = [
  "/usr/local/include/rocksdb";
  "/usr/include/rocksdb";
]
|> List.map ~f:(fun s -> [ "-I" ^ s])
in

let include_test = {|
#include <c.h>

int main() {
  rocksdb_options_t* opt = rocksdb_options_create();
  rocksdb_options_destroy(opt);
  return 0;
};|} in

let c_flags = List.find known_includes ~f:(fun c_flags -> C.c_test c ~c_flags ~link_flags include_test) in

match c_flags with
| Some c_flags ->
   C.Flags.write_sexp "c_flags.sexp"         c_flags;
   C.Flags.write_sexp "c_library_flags.sexp" link_flags
| None ->
   eprintf "failed to find an include path for RocksDB: are development headers installed on your system ?\n";
   eprintf "tested paths: %s\n" (List.concat known_includes |> String.concat ~sep:" ");
   C.die "discover error"

end

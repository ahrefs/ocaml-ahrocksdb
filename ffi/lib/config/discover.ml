open Printf

let minimum_rocks_version = "5.14"

module C = Configurator.V1

let () = C.main ~name:"librocksdb" begin fun c ->
let link_flags = ["-lrocksdb"] in

let known_paths = [
  "/usr/local/include/rocksdb";
  "/usr/include/rocksdb";
] in

let include_test = {|

#include <c.h>
#include <version.h>

int main() {
  rocksdb_options_t* opt = rocksdb_options_create();
  rocksdb_options_destroy(opt);
  return 0;
};

|}
in

let c_flag = List.find_opt (fun c_flag -> C.c_test c ~c_flags:["-I" ^ c_flag] ~link_flags include_test) known_paths in

match c_flag with
| None ->

   eprintf "failed to find an include path for RocksDB: are development headers installed on your system ?\n";
   eprintf "tested paths: %s\n" (String.concat " " known_paths);
   C.die "discover error"

| Some c_flag -> try

   let version_path = c_flag ^ "/version.h" in
   let assoc = C.C_define.import c ~includes:[ version_path ] ["ROCKSDB_MAJOR", Int; "ROCKSDB_MINOR", Int] in
   let expect_int name =
     match List.assoc_opt name assoc with
     | Some (Int i) -> i
     | Some _ -> failwith (sprintf "%s is not an int in %s" name version_path)
     | None -> failwith (sprintf "could not find %s in %s" name version_path)
   in

   let major = expect_int "ROCKSDB_MAJOR" in
   let minor = expect_int "ROCKSDB_MINOR" in
   let version = sprintf "%d.%d" major minor in
   if (String.compare minimum_rocks_version version) > 0 then failwith (sprintf "installed RocksDB installation is too old: found %s, expected %s minimum" version minimum_rocks_version);

   C.Flags.write_sexp "c_flags.sexp"         ["-I" ^ c_flag];
   C.Flags.write_sexp "c_library_flags.sexp" link_flags;
   C.Flags.write_lines "c_flags.txt"         ["-I" ^ c_flag];
   C.Flags.write_lines "c_library_flags.txt" link_flags

  with Failure s -> C.die "failure: %s" s

end

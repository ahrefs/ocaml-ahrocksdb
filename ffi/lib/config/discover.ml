open Printf

let minimum_rocks_major, minimum_rocks_minor = 5, 14

module C = Configurator.V1

let () = C.main ~name:"librocksdb" begin fun c ->

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


let known_paths = [
  "/usr/local/include/rocksdb";
  "/usr/include/rocksdb";
] in

(* When building statically, generate a rocksdb.pc in the build output dir
   with the actual installed version and only the compression libraries
   present on this system, then point PKG_CONFIG_PATH at that dir. *)
let include_dir =
  match List.find_opt (fun p -> Sys.file_exists (p ^ "/version.h")) known_paths with
  | Some dir -> dir
  | None ->
    eprintf "failed to locate rocksdb/version.h; are dev headers installed?\n";
    C.die "discover error"
in
let major, minor =
  (try
    let version_path = include_dir ^ "/version.h" in
    let assoc = C.C_define.import c
        ~c_flags:["-O0"; "-x"; "c++"] ~includes:[version_path]
        ["ROCKSDB_MAJOR", Int; "ROCKSDB_MINOR", Int] in
    let get_int name = match List.assoc_opt name assoc with
      | Some (Int i) -> i
      | Some _ -> failwith (sprintf "%s is not an int" name)
      | None -> failwith (sprintf "could not find %s" name)
    in
    (get_int "ROCKSDB_MAJOR", get_int "ROCKSDB_MINOR")
  with Failure s -> C.die "failure: %s" s)
in
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
let oc = open_out (Filename.concat pc_dir "rocksdb.pc") in
fprintf oc "Name: rocksdb\nDescription: RocksDB static override\nVersion: %d.%d\nLibs: -lrocksdb\nLibs.private: %s\nCflags:\n"
  major minor (String.concat " " libs_private);
close_out oc;
let existing = match Sys.getenv_opt "PKG_CONFIG_PATH" with
  | Some s -> ":" ^ s
  | None -> ""
in
let () = Unix.putenv "PKG_CONFIG_PATH" (pc_dir ^ existing) in
let () = Unix.putenv "PKG_CONFIG_ARGN" "--static" in

(* Try pkg-config first; fall back to manual path search *)
let c_flags, link_flags =
  let pkg_result =
    match C.Pkg_config.get c with
    | None -> None
    | Some pkg -> C.Pkg_config.query pkg ~package:"rocksdb"
  in
  match pkg_result with
  | Some { C.Pkg_config.libs; cflags } -> (cflags, libs)
  | None ->
    let path = List.find_opt (fun p ->
      C.c_test c ~c_flags:["-I" ^ p; "-x"; "c++"] ~link_flags:["-lrocksdb"] include_test
    ) known_paths in
    begin match path with
    | None ->
      eprintf "failed to find an include path for RocksDB: are development headers installed?\n";
      eprintf "tested paths: %s\n" (String.concat " " known_paths);
      C.die "discover error"
    | Some path -> (["-I" ^ path], ["-lrocksdb"])
    end
in

(* Locate the directory containing version.h for the version check *)
let include_dir =
  match
    let from_flags = List.filter_map (fun f ->
      if String.length f > 2 && String.sub f 0 2 = "-I" then begin
        let dir = String.sub f 2 (String.length f - 2) in
        if Sys.file_exists (dir ^ "/version.h") then Some dir
        else if Sys.file_exists (dir ^ "/rocksdb/version.h") then Some (dir ^ "/rocksdb")
        else None
      end else None) c_flags
    in
    match from_flags with
    | dir :: _ -> Some dir
    | [] -> List.find_opt (fun p -> Sys.file_exists (p ^ "/version.h")) known_paths
  with
  | Some dir -> dir
  | None ->
    eprintf "failed to locate rocksdb/version.h\n";
    C.die "discover error"
in

(* pkg-config may omit -I when the path is a default system include dir,
   but the C stubs use #include <c.h> (not <rocksdb/c.h>) so the rocksdb
   directory itself must be in the include path *)
let include_flag = "-I" ^ include_dir in
let c_flags =
  if List.mem include_flag c_flags then c_flags
  else include_flag :: c_flags
in

(* version.h includes <string> so C++ is required to parse it *)
(try
  let version_path = include_dir ^ "/version.h" in
  let assoc = C.C_define.import c ~c_flags:["-O0"; "-x"; "c++"] ~includes:[version_path]
                ["ROCKSDB_MAJOR", Int; "ROCKSDB_MINOR", Int] in
  let expect_int name =
    match List.assoc_opt name assoc with
    | Some (Int i) -> i
    | Some _ -> failwith (sprintf "%s is not an int in %s" name version_path)
    | None -> failwith (sprintf "could not find %s in %s" name version_path)
  in
  let major = expect_int "ROCKSDB_MAJOR" in
  let minor = expect_int "ROCKSDB_MINOR" in
  if (major, minor) < (minimum_rocks_major, minimum_rocks_minor) then
    failwith (sprintf "RocksDB too old: found %d.%d, expected >= %d.%d"
      major minor minimum_rocks_major minimum_rocks_minor)
with Failure s -> C.die "failure: %s" s);

(* c_library_flags carries all rocksdb deps in both modes.  The DLL build
   resolves -lrocksdb to librocksdb.so (no -Wl,-Bstatic in effect there),
   so no non-PIC .a is linked into a shared object and mold is satisfied.
   Executables that need static linking supply -Wl,-Bstatic themselves via
   -cc in their own link_flags; that flag only affects their link step. *)
C.Flags.write_sexp  "c_flags.sexp"          c_flags;
C.Flags.write_sexp  "c_library_flags.sexp"  link_flags;
C.Flags.write_lines "c_flags.txt"           c_flags;
C.Flags.write_lines "c_library_flags.txt"   link_flags;

end

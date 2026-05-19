open Printf

let minimum_rocks_major, minimum_rocks_minor = 5, 14

module C = Configurator.V1

let () = C.main ~name:"librocksdb" begin fun c ->

(* Given a list of -I flags and known fallback paths, return the directory
   that directly contains version.h (the rocksdb include directory). *)
let find_include_dir c_flags known_paths =
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
in

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


let static = match Sys.getenv_opt "ROCKSDB_LINK_MODE" with
  | Some "static" -> true
  | _ -> false
in

let known_paths = [
  "/usr/local/include/rocksdb";
  "/usr/include/rocksdb";
] in

(* When building statically, prepend our local pkgconfig/ override directory
   so that it takes precedence over the system rocksdb.pc, which typically
   omits Libs.private (the transitive static deps).
   INSIDE_DUNE is set by dune to the context build directory (e.g.
   /abs/project/_build/default); two dirname steps reach the workspace root. *)
if static then begin
  let workspace_root =
    match Sys.getenv_opt "INSIDE_DUNE" with
    | Some build_dir -> Filename.dirname (Filename.dirname build_dir)
    | None -> Sys.getcwd ()
  in
  let local_pc_dir = Filename.concat workspace_root "pkgconfig" in
  if Sys.file_exists local_pc_dir then begin
    let existing = match Sys.getenv_opt "PKG_CONFIG_PATH" with
      | Some s -> ":" ^ s
      | None -> ""
    in
    Unix.putenv "PKG_CONFIG_PATH" (local_pc_dir ^ existing)
  end;
  (* PKG_CONFIG_ARGN is read by Pkg_config.get and prepended to every query *)
  Unix.putenv "PKG_CONFIG_ARGN" "--static"
end;

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
  match find_include_dir c_flags known_paths with
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

C.Flags.write_sexp  "c_flags.sexp"         c_flags;
C.Flags.write_sexp  "c_library_flags.sexp" link_flags;
C.Flags.write_lines "c_flags.txt"          c_flags;
C.Flags.write_lines "c_library_flags.txt"  link_flags

end

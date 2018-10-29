let prologue = "
#include <c.h>
"

let () =
  print_endline prologue;
  Cstubs_structs.write_c Format.std_formatter (module Rocksdb_types.Struct_stubs)

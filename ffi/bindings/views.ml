open Ctypes

let bool_to_uchar =
  let open Unsigned in
  view
    ~read:(fun u -> u <> UChar.zero)
    ~write:(function true -> UChar.one | false -> UChar.zero)
    uchar

let bool_to_int =
  view
    ~read:(fun u -> u <> 0)
    ~write:(function true -> 1 | false -> 0)
    int

let int_of_compression = function
  | `No_compression -> 0
  | `Snappy -> 1
  | `Zlib -> 2
  | `Bz2 -> 3
  | `Lz4 -> 4
  | `Lz4hc -> 5
  | `Xpress -> 6
  | `Zstd -> 7

let compression_view =
  let read = function
    | 0 -> `No_compression
    | 1 -> `Snappy
    | 2 -> `Zlib
    | 3 -> `Bz2
    | 4 -> `Lz4
    | 5 -> `Lz4hc
    | 6 -> `Xpress
    | 7 -> `Zstd
    | other -> invalid_arg @@ Printf.sprintf "read_compression_view: invalid compression type: %d" other
  in
  let write = int_of_compression in
  Ctypes.view ~read ~write Ctypes.int

let int_to_uint64 =
  let open Unsigned in
  view ~write:UInt64.of_int ~read:UInt64.to_int Ctypes.uint64_t

let int_to_size_t =
  let open Unsigned in
  view ~write:Size_t.of_int ~read:Size_t.to_int Ctypes.size_t

let size_t_to_int =
  let open Unsigned in
  view ~write:Size_t.to_int ~read:Size_t.of_int Ctypes.int

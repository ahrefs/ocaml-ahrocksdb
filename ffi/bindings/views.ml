open Ctypes

let bool_to_uchar =
  let open Unsigned in
  view
    ~read:(fun u -> u <> UChar.zero)
    ~write:(function true -> UChar.one | false -> UChar.zero)
    uchar

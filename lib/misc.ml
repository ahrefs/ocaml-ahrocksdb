module Opt = struct

  let iter o f =
    match o with
    | Some v -> f v
    | None -> ()

  let get_or ~default = function
    | Some value -> value
    | None -> Lazy.force default

  let (>>=) = iter

end

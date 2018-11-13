open Printf

module type Wrap_t = sig

  type t
  val kind : string

end

module Wrap (M : Wrap_t) = struct

  type t = {
    mutable valid : bool;
    content : M.t;
    kind : string;
  }

  let wrap v = { valid = true; content = v; kind = M.kind; }

  let unwrap t f =
    match t with
    | { valid = false; kind; _ } -> Error (`Msg (sprintf "trying to access closed %s handle" kind))
    | { valid = true; content; _ } -> f content

  let on_finalise t f = match unwrap t (fun t -> Ok (f t)) with _ -> t.valid <- false

  let (>>=) = Rresult.R.Infix.(>>=)

end

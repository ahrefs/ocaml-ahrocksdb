module Ffi = Rocksdb_ffi.M
module Rocksdb = Ffi.Rocksdb

open Ffi

type t = PerfContext.t

let create () =
  let t = PerfContext.create () in
  Gc.finalise PerfContext.destroy t;
  t

let reset = PerfContext.reset

module Metrics = struct

  include PerfContext.Counters

end

let metric = PerfContext.metric

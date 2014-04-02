(* The benchmark shows ~6x faster complex number arithmetic using hardware primitives. *)

let () =
  let n = 1_000_000 in
  let start = Unix.gettimeofday () in
  let a = { Complex.re = 1.; im = 2. } in
  let b = { Complex.re = 2.; im = 0.5 } in
  for i = 1 to n do
    let c = Complex.add a b in
    let d = Complex.sub a b in
    let _ = Complex.mul c d in
    ()
  done;
  let t = Unix.gettimeofday () -. start in
  let start = Unix.gettimeofday () in
  let module Ci = Complex_intrin in
  let a = Ci.create 1. 2. in
  let b = Ci.create 2. 0.5 in
  for i = 1 to n do
    let c = Ci.add a b in
    let d = Ci.sub a b in
    let _ = Ci.mul c d in
    ()
  done;
  Printf.printf "speedup %f\n" (t /. (Unix.gettimeofday () -. start))

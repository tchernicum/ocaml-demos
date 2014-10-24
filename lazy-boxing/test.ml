let base x =
  let s = ref 0. in
  for i = 1 to 1_000_000_000 do ()
  done;
  !s

let sum1 x =
  let s = ref 0. in
  for i = 1 to 1_000_000_000 do
    s := !s +. x
  done;
  !s +. 0.

let sum2 x =
  let s = ref 0. in
  for i = 1 to 1_000_000_000 do
    s := !s +. x
  done;
  !s

let () =
  let t0 = Unix.gettimeofday () in
  let _ = sum1 1. in
  let t1 = Unix.gettimeofday () in
  let _ = sum2 1. in
  let t2 = Unix.gettimeofday () in
  let _ = base 1. in
  let t3 = Unix.gettimeofday () in
  let t3 = t3 -. t2 in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  let t2 = t2 -. t3 in
  let t1 = t1 -. t3 in
  Printf.printf "%f %f %f\n" t1 t2 (t2 /. t1)

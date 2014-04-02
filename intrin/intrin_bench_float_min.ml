(* The bench shows 9.3 times faster float [min] using hardware primitives. *)

external float_min' : float -> float -> float = "%asm" "minsd	%2, %0" "0x" "mmx" ""

let is_nan x = (x : float) <> x
let float_min (x : float) y =
  if is_nan x || is_nan y then nan
  else if x < y then x else y

let () =
  let n = 1000 in
  let m = 1000 in
  let a = Array.init m (fun _ -> Random.float 1.) in
  let start = Unix.gettimeofday () in
  for i = 1 to n do
    for j = 0 to m - 2 do
      let x = Array.unsafe_get a j in
      let y = Array.unsafe_get a (j + 1) in
      let _ = float_min x y in ()
    done
  done;
  let t = Unix.gettimeofday () -. start in
  let start = Unix.gettimeofday () in
  for i = 1 to n do
    for j = 0 to m - 2 do
      let x = Array.unsafe_get a j in
      let y = Array.unsafe_get a (j + 1) in
      let _ = float_min' x y in ()
    done
  done;
  Printf.printf "speedup %f\n" (t /. (Unix.gettimeofday () -. start))

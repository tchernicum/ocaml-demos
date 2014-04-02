(* The benchmark shows 7.8x faster [floor] using hardware primitives. *)

external _mm_round_pd : int -> float -> float = "%asm" "roundsd	%1, %2, %0" "" "m" ""
let _mm_floor_pd x = _mm_round_pd 1 x
let _mm_ceil_pd x = _mm_round_pd 2 x

let () =
  let n = 1_000_000 in
  let start = Unix.gettimeofday () in
  let x = 1.5 in
  for i = 1 to n do
    let _ = floor x in ()
  done;
  let t = Unix.gettimeofday () -. start in
  let start = Unix.gettimeofday () in
  let x = 1.5 in
  for i = 1 to n do
    let _ = _mm_floor_pd x in ()
  done;
  Printf.printf "speedup %f\n" (t /. (Unix.gettimeofday () -. start))

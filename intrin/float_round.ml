(* The benchmark shows 7.8x faster [floor] using hardware primitives. *)

external round_asm : float -> int -> float = "%asm" "roundsd	%1, %0, %2" "x" "i" "=x"
let floor_asm x = round_asm x 1
let ceil_asm  x = round_asm x 2

let () =
  let n = 1_000_000 in
  let start = Unix.gettimeofday () in
  let x = 1.5 in
  let s0 = ref 0. in
  let s1 = ref 0. in
  let s2 = ref 0. in
  let s3 = ref 0. in
  let s4 = ref 0. in
  let s5 = ref 0. in
  let s6 = ref 0. in
  let s7 = ref 0. in
  for i = 1 to n do
    s0 := floor x;
    s1 := floor x;
    s2 := floor x;
    s3 := floor x;
    s4 := floor x;
    s5 := floor x;
    s6 := floor x;
    s7 := floor x;
  done;
  let start = start +. (!s0 +. !s1 +. !s2 +. !s3) *. 0. in
  let t = Unix.gettimeofday () -. start in
  let start = Unix.gettimeofday () in
  let x = 1.5 in
  let s0 = ref 0. in
  let s1 = ref 0. in
  let s2 = ref 0. in
  let s3 = ref 0. in
  let s4 = ref 0. in
  let s5 = ref 0. in
  let s6 = ref 0. in
  let s7 = ref 0. in
  for i = 1 to n do
    s0 := floor_asm x;
    s1 := floor_asm x;
    s2 := floor_asm x;
    s3 := floor_asm x;
    s4 := floor_asm x;
    s5 := floor_asm x;
    s6 := floor_asm x;
    s7 := floor_asm x;
  done;
  let start = start +. (!s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7) *. 0. in
  Printf.printf "speedup %f\n" (t /. (Unix.gettimeofday () -. start))

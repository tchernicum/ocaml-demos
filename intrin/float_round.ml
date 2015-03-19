(* The benchmark shows faster [floor] using hardware primitives. *)

external floor_asm : float -> float
  = "%asm" "floor_stub" "roundsd	$1, %0, %1	# floor" "mx" "=x"
external ceil_asm : float -> float
  = "%asm" "ceil_stub" "roundsd	$2, %0, %1	# ceil" "mx" "=x"

let () =
  let n = 1_000_000 in
  let t0 = Unix.gettimeofday () in
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
  let t0 = t0 +. (!s0 +. !s1 +. !s2 +. !s3) *. 0. in
  let t1 = Unix.gettimeofday () in
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
  let t1 = t1 +. (!s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7) *. 0. in
  let t2 = Unix.gettimeofday () in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  let ns_mult = 1_000_000_000. /. (8. *. float_of_int n) in
  Printf.printf "%f ns vs %f ns, speedup %f\n"
    (t1 *. ns_mult) (t2 *. ns_mult) (t1 /. t2)

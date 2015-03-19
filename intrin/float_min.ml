(* The benchmark shows faster float [min] using hardware primitives. *)

external float_min_asm : float -> float -> float
  = "%asm" "float_min_asm_stub" "minsd	%0, %2" "mx" "2" "=x"

let float_min (x : float) y = if x < y then x else y

let () =
  let n = 1000 in
  let m = 1000 in
  let a = Array.init m (fun _ -> Random.float 1.) in
  let t0 = Unix.gettimeofday () in
  let s = ref 0. in
  for i = 1 to n do
    let s0 = ref 0. in
    let s1 = ref 0. in
    let s2 = ref 0. in
    let s3 = ref 0. in
    let s4 = ref 0. in
    let s5 = ref 0. in
    let s6 = ref 0. in
    let s7 = ref 0. in
    for j = 0 to m - 2 do
      let x = Array.unsafe_get a j in
      let y = Array.unsafe_get a (j + 1) in
      s0 := float_min x y;
      s1 := float_min x y;
      s2 := float_min x y;
      s3 := float_min x y;
      s4 := float_min x y;
      s5 := float_min x y;
      s6 := float_min x y;
      s7 := float_min x y;
    done;
    s := !s +. !s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7
  done;
  let t1 = Unix.gettimeofday () in
  for i = 1 to n do
    let s0 = ref 0. in
    let s1 = ref 0. in
    let s2 = ref 0. in
    let s3 = ref 0. in
    let s4 = ref 0. in
    let s5 = ref 0. in
    let s6 = ref 0. in
    let s7 = ref 0. in
    for j = 0 to m - 2 do
      let x = Array.unsafe_get a j in
      let y = Array.unsafe_get a (j + 1) in
      s0 := float_min_asm x y;
      s1 := float_min_asm x y;
      s2 := float_min_asm x y;
      s3 := float_min_asm x y;
      s4 := float_min_asm x y;
      s5 := float_min_asm x y;
      s6 := float_min_asm x y;
      s7 := float_min_asm x y;
    done;
    s := !s +. !s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7
  done;
  let t1 = t1 +. !s *. 0. in
  let t2 = Unix.gettimeofday () in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  let ns_mult = 1_000_000_000. /. (8. *. float_of_int (n * (m - 2))) in
  Printf.printf "%f ns vs %f ns, speedup %f\n"
    (t1 *. ns_mult) (t2 *. ns_mult) (t1 /. t2)

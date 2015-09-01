(* The benchmark shows faster float [min] using hardware primitives. *)

external float_min_asm : float -> float -> float
  = "%asm" "float_min_asm_stub" "minsd	%1, %2" "2" "xm64" "=x"

external float_min_c : float -> float -> float = "float_min_stub" "float_min_stub" "float"

let float_min (x : float) y = if x < y then x else y

let () =
  let n = 1000 in
  let m = 1000 in
  let a = Array.init m (fun _ -> Random.float 1.) in
  let t0 = Unix.gettimeofday () in
  let s = ref 0. in
  for i = 1 to n do
    let s0 = ref 0. in
    for j = 0 to m - 2 do
      let x = Array.unsafe_get a j in
      let y = Array.unsafe_get a (j + 1) in
      s0 := float_min x y;
      s0 := float_min x y;
      s0 := float_min x y;
      s0 := float_min x y
    done;
    s := !s +. !s0
  done;
  let t0 = t0 +. !s *. 0. in
  let t1 = Unix.gettimeofday () in
  for i = 1 to n do
    let s0 = ref 0. in
    for j = 0 to m - 2 do
      let x = Array.unsafe_get a j in
      let y = Array.unsafe_get a (j + 1) in
      s0 := float_min_asm x y;
      s0 := float_min_asm x y;
      s0 := float_min_asm x y;
      s0 := float_min_asm x y;
    done;
    s := !s +. !s0
  done;
  let t1 = t1 +. !s *. 0. in
  let t2 = Unix.gettimeofday () in
  for i = 1 to n do
    let s0 = ref 0. in
    for j = 0 to m - 2 do
      let x = Array.unsafe_get a j in
      let y = Array.unsafe_get a (j + 1) in
      s0 := float_min_c x y;
      s0 := float_min_c x y;
      s0 := float_min_c x y;
      s0 := float_min_c x y;
    done;
    s := !s +. !s0
  done;
  let t2 = t2 +. !s *. 0. in
  let t3 = Unix.gettimeofday () in
  for i = 1 to n do
    let s0 = ref 0. in
    for j = 0 to m - 2 do
      let _ = Array.unsafe_get a j in
      let _ = Array.unsafe_get a (j + 1) in ()
    done;
    s := !s +. !s0
  done;
  let t3 = t3 +. !s *. 0. in
  let t4 = Unix.gettimeofday () in
  let t4 = t4 -. t3 in
  let t3 = t3 -. t2 -. t4 in
  let t2 = t2 -. t1 -. t4 in
  let t1 = t1 -. t0 -. t4 in
  let ns_mult = 1_000_000_000. /. (float_of_int (n * (m - 2))) in
  Printf.printf "float_min   %6.2f ns vs %6.2f ns, speedup %6.2f\n"
    (t1 *. ns_mult) (t2 *. ns_mult) (t1 /. t2);
  Printf.printf "float_min_c %6.2f ns vs %6.2f ns, speedup %6.2f\n"
    (t3 *. ns_mult) (t2 *. ns_mult) (t3 /. t2)

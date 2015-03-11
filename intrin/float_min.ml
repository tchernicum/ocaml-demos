(* The bench shows 9.3 times faster float [min] using hardware primitives. *)

external float_min_asm : float -> float -> float = "%asm" "minsd	%0, %2" "mx" "2" "=x"

let is_nan x = (x : float) <> x
let float_min (x : float) y =
  if is_nan x || is_nan y then nan
  else if x < y then x else y

let () =
  let n = 1000 in
  let m = 1000 in
  let a = Array.init m (fun _ -> Random.float 1.) in
  let start = Unix.gettimeofday () in
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
  let t = Unix.gettimeofday () -. start in
  let start = Unix.gettimeofday () in
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
  let start = start +. !s *. 0. in
  Printf.printf "speedup %f\n" (t /. (Unix.gettimeofday () -. start))

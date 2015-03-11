(* The benchmark shows 2x faster [float] arithmetics using three operand hardware
   primitives. *)

module Three_operand_float = struct
  external ( +. ) : float -> float -> float    = "%asm" "vaddps	%0, %1, %2" "x" "xm" "=x"
  external ( *. ) : float -> float -> float    = "%asm" "vmulps	%0, %1, %2" "x" "xm" "=x"
end

let () =
  let n = 100_000_000 in
  let t0 = Unix.gettimeofday () in
  let zero = 0.0 in
  let a = 1.0 +. zero in
  let b = 1.0 +. zero in
  let c = 1.0 +. zero in
  let d = 1.0 +. zero in
  let s0 = ref 0. in
  let s1 = ref 0. in
  let s2 = ref 0. in
  let s3 = ref 0. in
  let s4 = ref 0. in
  let s5 = ref 0. in
  let s6 = ref 0. in
  let s7 = ref 0. in
  for i = 1 to n do
    s0 := a +. b;
    s1 := c *. d;
    s2 := a +. b;
    s3 := c *. d;
    s4 := a +. b;
    s5 := c *. d;
    s6 := a +. b;
    s7 := c *. d;
  done;
  let t0 = t0 +. (!s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7) *. 0. in
  let t1 = Unix.gettimeofday () in
  let open Three_operand_float in
  let zero = 0.0 in
  let a = 1.0 +. zero in
  let b = 1.0 +. zero in
  let c = 1.0 +. zero in
  let d = 1.0 +. zero in
  let s0 = ref 0. in
  let s1 = ref 0. in
  let s2 = ref 0. in
  let s3 = ref 0. in
  let s4 = ref 0. in
  let s5 = ref 0. in
  let s6 = ref 0. in
  let s7 = ref 0. in
  for i = 1 to n do
    s0 := a +. b;
    s1 := c *. d;
    s2 := a +. b;
    s3 := c *. d;
    s4 := a +. b;
    s5 := c *. d;
    s6 := a +. b;
    s7 := c *. d;
  done;
  let t1 = t1 +. (!s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7) *. 0. in
  let t2 = Unix.gettimeofday () in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  Printf.printf "speedup %f\n" (t1 /. t2)

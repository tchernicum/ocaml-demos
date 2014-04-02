(* The benchmark shows 2x faster [float] arithmetics using three operand hardware
   primitives. *)

module Three_operand_float = struct
  external ( +. ) : float -> float -> float    = "%asm" "vaddps	%1, %2, %0" "mmx" "x" ""
  external ( *. ) : float -> float -> float    = "%asm" "vmulps	%1, %2, %0" "mmx" "x" ""
end

let () =
  let n = 100_000_000 in
  let t0 = Unix.gettimeofday () in
  let zero = 0.0 in
  let a = 1.0 +. zero in
  let b = 1.0 +. zero in
  let c = 1.0 +. zero in
  let d = 1.0 +. zero in
  for i = 1 to n do
    let _ = a +. b in
    let _ = c *. d in
    let _ = a +. b in
    let _ = c *. d in
    let _ = a +. b in
    let _ = c *. d in
    let _ = a +. b in
    let _ = c *. d in
    ()
  done;
  let t1 = Unix.gettimeofday () in
  let open Three_operand_float in
  let zero = 0.0 in
  let a = 1.0 +. zero in
  let b = 1.0 +. zero in
  let c = 1.0 +. zero in
  let d = 1.0 +. zero in
  for i = 1 to n do
    let _ = a +. b in
    let _ = c *. d in
    let _ = a +. b in
    let _ = c *. d in
    let _ = a +. b in
    let _ = c *. d in
    let _ = a +. b in
    let _ = c *. d in
    ()
  done;
  let t2 = Unix.gettimeofday () in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  Printf.printf "speedup %f\n" (t1 /. t2)

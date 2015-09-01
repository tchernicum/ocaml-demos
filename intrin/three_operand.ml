(* The benchmark shows 2x faster [float] arithmetics using three operand hardware
   primitives. *)

module Two_operand_float = struct
  external ( +. ) : float -> float -> float = "%asm" "" "addsd	%1, %2" "%2" "xm64" "=x"
  external ( -. ) : float -> float -> float = "%asm" "" "subsd	%1, %2" "2" "xm64" "=x"
  external ( *. ) : float -> float -> float = "%asm" "" "mulsd	%1, %2" "%2" "xm64" "=x"
  external ( /. ) : float -> float -> float = "%asm" "" "divsd	%1, %2" "2" "xm64" "=x"
end

module Three_operand_float = struct
  external ( +. ) : float -> float -> float = "%asm" "" "vaddsd	%1, %0, %2" "%x" "xm64" "=x"
  external ( -. ) : float -> float -> float = "%asm" "" "vsubsd	%1, %0, %2"  "x" "xm64" "=x"
  external ( *. ) : float -> float -> float = "%asm" "" "vmulsd	%1, %0, %2" "%x" "xm64" "=x"
  external ( /. ) : float -> float -> float = "%asm" "" "vdivsd	%1, %0, %2"  "x" "xm64" "=x"
end

let () =
  let module T = Three_operand_float in
  let n = 1024 in
  let a = Array.init n (fun _ -> Random.float 1.) in
  for i = 0 to n - 1 do
    let x = Array.unsafe_get a i in
    for j = 0 to n - 1 do
      let y = Array.unsafe_get a j in
      assert (x +. y = T.(x +. y));
      assert (x -. y = T.(x -. y));
      assert (x *. y = T.(x *. y));
      assert (x /. y = T.(x /. y));
      assert (x +. Array.unsafe_get a j = T.(x +. Array.unsafe_get a j));
      assert (x -. Array.unsafe_get a j = T.(x -. Array.unsafe_get a j));
      assert (x *. Array.unsafe_get a j = T.(x *. Array.unsafe_get a j));
      assert (x /. Array.unsafe_get a j = T.(x /. Array.unsafe_get a j));
      assert (Array.unsafe_get a j +. x = T.(Array.unsafe_get a j +. x));
      assert (Array.unsafe_get a j -. x = T.(Array.unsafe_get a j -. x));
      assert (Array.unsafe_get a j *. x = T.(Array.unsafe_get a j *. x));
      assert (Array.unsafe_get a j /. x = T.(Array.unsafe_get a j /. x));
    done
  done

let () =
  let n = 100_000_000 in
  let t0 = Unix.gettimeofday () in
  let zero = ref 0.0 in
  let a = 1.0 +. !zero in
  let b = 1.0 +. !zero in
  let c = 1.0 +. !zero in
  let d = 1.0 +. !zero in
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
    s1 := a *. b;
    s2 := a +. c;
    s3 := a *. c;
    s4 := a +. d;
    s5 := a *. d;
    s6 := b +. c;
    s7 := b *. c;
  done;
  let t0 = t0 +. (!s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7) *. 0. in
  let t1 = Unix.gettimeofday () in
  let open Three_operand_float in
  let zero = ref 0.0 in
  let a = 1.0 +. !zero in
  let b = 1.0 +. !zero in
  let c = 1.0 +. !zero in
  let d = 1.0 +. !zero in
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
    s1 := a *. b;
    s2 := a +. c;
    s3 := a *. c;
    s4 := a +. d;
    s5 := a *. d;
    s6 := b +. c;
    s7 := b *. c;
  done;
  let t1 = t1 +. (!s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7) *. 0. in
  let t2 = Unix.gettimeofday () in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  let ns_mult = 1_000_000_000. /. (float_of_int (8 * n)) in
  Printf.printf "%f ns vs %f ns, speedup %f\n"
    (t1 *. ns_mult) (t2 *. ns_mult) (t1 /. t2)

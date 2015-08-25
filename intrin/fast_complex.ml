(* The benchmark shows ~6x faster complex number arithmetic using hardware primitives. *)

module Complex_asm = struct
  type m128d
  type t = m128d

  external _mm_add_pd : m128d -> m128d -> m128d = "%asm" ""
       "addpd	%1, %2	# _mm_add_pd" "%2" "xm128" "=x"
  external _mm_sub_pd : m128d -> m128d -> m128d = "%asm" ""
       "subpd	%1, %2	# _mm_sub_pd" "2" "xm128" "=x"
  external _mm_mul_pd : m128d -> m128d -> m128d = "%asm" ""
       "mulpd	%1, %2	# _mm_mul_pd" "%2" "xm128" "=x"
  external _mm_div_pd : m128d -> m128d -> m128d = "%asm" ""
       "divpd	%1, %2	# _mm_div_pd" "2" "xm128" "=x"
  external _mm_shuffle_pd : m128d -> m128d -> int -> m128d = "%asm" ""
       "shufpd	%2, %1, %3	# _mm_shuffle_pd" "3" "xm128" "i" "=x"
  external _mm_addsub_pd : m128d -> m128d -> m128d = "%asm" ""
       "addsubpd	%1, %2	# _mm_addsub_pd" "2" "xm128" "=x"
  external _mm_set_pd : float -> float -> m128d = "%asm" ""
       "unpcklpd	%1, %2	# _mm_set_pd" "2" "xm128" "=x"

  let create re im = _mm_set_pd re im
  let add x y = _mm_add_pd x y
  let sub x y = _mm_sub_pd x y
  let mul ab cd =
    let ac_bd = _mm_mul_pd ab cd in
    let ba = _mm_shuffle_pd ab ab 1 in
    let bc_ad = _mm_mul_pd ba cd in
    _mm_addsub_pd ac_bd bc_ad
end

module Complex_c = struct
  type t = { re : float; im : float }

  let create re im = { re; im }
  external add : t -> t -> t = "complex_add"
  external sub : t -> t -> t = "complex_sub"
  external mul : t -> t -> t = "complex_mul"
end

let () =
  let n = 1_000_000 in
  let t0 = Unix.gettimeofday () in
  let a = { Complex.re = 1.; im = 2. } in
  let b = { Complex.re = 2.; im = 0.5 } in
  for i = 1 to n do
    let c = Complex.add a b in
    let d = Complex.sub a b in
    let _ = Complex.mul c d in
    ()
  done;
  let t1 = Unix.gettimeofday () in
  let module C = Complex_asm in
  let a = C.create 1. 2. in
  let b = C.create 2. 0.5 in
  for i = 1 to n do
    let c = C.add a b in
    let d = C.sub a b in
    let _ = C.mul c d in
    ()
  done;
  let t2 = Unix.gettimeofday () in
  let a = Complex_c.create 1. 2. in
  let b = Complex_c.create 2. 0.5 in
  for i = 1 to n do
    let c = Complex_c.add a b in
    let d = Complex_c.sub a b in
    let _ = Complex_c.mul c d in
    ()
  done;
  let t3 = Unix.gettimeofday () in
  let t3 = t3 -. t2 in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  let ns_mult = 1_000_000_000. /. (float_of_int n) in
  Printf.printf "Complex    %6.2f ns vs %6.2f ns, speedup %6.2f\n"
    (t1 *. ns_mult) (t2 *. ns_mult) (t1 /. t2);
  Printf.printf "Complex_c  %6.2f ns vs %6.2f ns, speedup %6.2f\n"
    (t3 *. ns_mult) (t2 *. ns_mult) (t3 /. t2)

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
  external _mm_hadd_pd : m128d -> m128d -> m128d = "%asm" ""
       "haddpd	%1, %2	# _mm_hadd_pd" "%2" "xm128" "=x"
  external to_float : m128d -> float          = "%asm" ""
       "movapd	%0, %1	# M128d.to_float" "x" "=x"

  let create re im = _mm_set_pd re im
  let add x y = _mm_add_pd x y
  let sub x y = _mm_sub_pd x y
  let mul ab cd =
    let ac_bd = _mm_mul_pd ab cd in
    let ba = _mm_shuffle_pd ab ab 1 in
    let bc_ad = _mm_mul_pd ba cd in
    _mm_addsub_pd ac_bd bc_ad
  let norm2 x =
    let xx = _mm_mul_pd x x in
    to_float (_mm_hadd_pd xx xx)
  let zero = create 0. 0.
end

open Complex_asm

let rec mandelbrot x y =
  let c = create x y in
  let z = ref (create 0. 0.) in
  let i = ref 0 in
  while !i <= 63 && norm2 !z > 4. do
    incr i;
    z := add (mul !z !z) c
  done;
  !i

let display () =
  let w, h = 640, 640 in
  let w_inv = 1. /. float w in
  let h_inv = 1. /. float h in
  for a = 0 to w - 1 do
    for b = 0 to h - 1 do
      let x = 4. *. float a *. w_inv -. 2. in
      let y = 4. *. float b *. h_inv -. 2. in
      let _ = mandelbrot x y in ()
    done;
  done

let () =
  let t0 = Unix.gettimeofday () in
  for i = 0 to 99 do
    display ()
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%f\n%!" (t1 -. t0)

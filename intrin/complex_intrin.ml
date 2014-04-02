type m128d
type t = m128d

external _mm_add_pd : m128d -> m128d -> m128d    = "%asm" "addpd	%2, %0" "0x" "mmx" ""
external _mm_sub_pd : m128d -> m128d -> m128d    = "%asm" "subpd	%2, %0" "0" "mm" ""
external _mm_mul_pd : m128d -> m128d -> m128d    = "%asm" "mulpd	%2, %0" "0x" "mmx" ""
external _mm_div_pd : m128d -> m128d -> m128d    = "%asm" "divpd	%2, %0" "0" "mm" ""
external _mm_shuffle_pd : m128d -> m128d -> int -> m128d = "%asm" "shufpd	%3, %0, %2" "0" "mm" "" ""
external _mm_addsub_pd : m128d -> m128d -> m128d    = "%asm" "addsubpd	%2, %0" "0x" "mmx" ""
external _mm_set_pd : float -> float -> m128d  = "%asm" "unpcklpd	%1, %0" "mm" "0" ""

let create re im = _mm_set_pd re im
let add x y = _mm_add_pd x y
let sub x y = _mm_sub_pd x y
let mul ab cd =
  let ac_bd = _mm_mul_pd ab cd in
  let ba = _mm_shuffle_pd ab ab 1 in
  let bc_ad = _mm_mul_pd ba cd in
  _mm_addsub_pd ac_bd bc_ad

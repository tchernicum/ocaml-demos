type m128d
type m128i

external _mm_add_sd : float -> float -> float =
  "%asm" "addpd	%2, %0" "0x" "mx" ""
external _addi : int -> int -> int = "%asm" 
	"xor	$1, %0
        add	%2, %0" "0x" "x" ""
external _mm_set_pd : float -> float -> m128d =
  "%asm" "unpcklpd	%1, %0" "mm" "0" ""
external _mm_shuffle_pd : m128d -> m128d -> int -> m128d =
  "%asm" "shufpd	%3, %2, %0" "0" "mm" "i" ""

external _mm_set_epi64 : int64 -> int64 -> m128i =
  "%asm" "movpd	%2, %0" "0" "mm" ""

let () =
  let x = _mm_add_sd 5. 2. in
  let y = _addi 5 3 in
  let z = _mm_set_pd 5. 2. in
  let _ = _mm_shuffle_pd z z 1 in
  Printf.printf "%f %d\n" x y

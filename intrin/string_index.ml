(* The benchmark shows 4-5x faster [String.index] using hardware primitives. *)

type m128i
type ptr

external index : string -> int -> ptr = "%asm" ""
       "sar	%2
	add	%0, %2" "r" "2" "=r"
external _mm_loadu_si128 : ptr -> m128i = "%asm" ""
       "movdqu	(%0), %1	# _mm_loadu_si128" "r" "=x"
external _mm_set1_epi64x : int64 -> m128i = "%asm" ""
       "movq	%0, %1" "r" "=x"
external _mm_pcmpestri : m128i -> int64 -> m128i -> int64 -> nativeint -> int64 =
  "%asm" "" "pcmpestri	%4, %2, %0" "x" "a" "mx" "d" "i" "=c"


external string_index_c : string -> char -> int = "string_index_stub" "noalloc"

let rec string_index_rec s l p c =
  let cc = _mm_set1_epi64x (Int64.of_int (Char.code c)) in
  let a = _mm_loadu_si128 (index s p) in
  let r = _mm_pcmpestri cc 1L a (Int64.of_int l) 0n in
  if r < 16L then p + Int64.to_int r
  else if l < 16 then -1
  else string_index_rec s (l - 16) (p + 16) c

let string_index s c =
  string_index_rec s (String.length s) 0 c

let () =
  let s = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let n = 1_000_000 in
  for code = 0x41 to 0x60 do
    let c = Char.chr code in
    let r = string_index s c in
    let r' = try String.index s c with Not_found -> -1 in
    assert (r = r')
  done;
  let t0 = Unix.gettimeofday () in
  for code = 0x41 to 0x60 do
    let c = Char.chr code in
    for i = 1 to n do
      let _ = try String.index s c with Not_found -> -1 in ()
    done
  done;
  let t1 = Unix.gettimeofday () in
  for code = 0x41 to 0x60 do
    let c = Char.chr code in
    for i = 1 to n do
      let _ = string_index s c in ()
    done
  done;
  let t2 = Unix.gettimeofday () in
  for code = 0x41 to 0x60 do
    let c = Char.chr code in
    for i = 1 to n do
      let _ = string_index_c s c in ()
    done
  done;
  let t3 = Unix.gettimeofday () in
  let t3 = t3 -. t2 in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  let ns_mult = 1_000_000_000. /. (float_of_int n) in
  Printf.printf "String.index   %7.2f ns vs %7.2f ns, speedup %7.2f\n"
    (t1 *. ns_mult) (t2 *. ns_mult) (t1 /. t2);
  Printf.printf "string_index_c %7.2f ns vs %7.2f ns, speedup %7.2f\n"
    (t3 *. ns_mult) (t2 *. ns_mult) (t3 /. t2)

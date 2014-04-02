(* The benchmark shows 4-5x faster [String.index] using hardware primitives. *)

type m128i_array

external _mm_loadu_si128 : m128i_array -> int -> m128i =
  "%asm" "movdqu	%1, %0" "" ""

external _mm_set1_epi64x : int64 -> m128i =
  "%asm" "movq	%1, %0" "m" ""

external _mm_pcmpestri : m128i -> int64 -> m128i -> int64 -> int -> int64 =
  "%asm" "pcmpestri	%5, %3, %1" "" "a" "mm" "d" "" "c"

let rec string_index_rec s l p c =
  let cc = _mm_set1_epi64x (Int64.of_int (Char.code c)) in
  let a = _mm_loadu_si128 s p in
  let r = _mm_pcmpestri cc (Int64.of_int 1) a (Int64.of_int l) 0 in
  if r < Int64.of_int 16 then (p * 16 + Int64.to_int r)
  else if l < 16 then -1
  else string_index_rec s (l - 16) (p + 1) c

let string_index s c =
  string_index_rec (Obj.magic s) (String.length s) 0 c

let () =
  let s = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  for code = 0x41 to 0x60 do
    let c = Char.chr code in
    let r = string_index s c in
    let r' = try String.index s c with Not_found -> -1 in
    assert (r = r');
    let t0 = Unix.gettimeofday () in
    for i = 1 to 1_000_000 do
      let _ = string_index s c in ()
    done;
    let t1 = Unix.gettimeofday () in
    for i = 1 to 1_000_000 do
      let _ = try String.index s c with Not_found -> -1 in ()
    done;
    let t2 = Unix.gettimeofday () in
    let t2 = t2 -. t1 in
    let t1 = t1 -. t0 in
    Printf.printf "%2d %f %f %f\n%!" r t1 t2 (t2 /. t1)
  done

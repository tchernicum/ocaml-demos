(* The benchmark shows 19x speedup of binomial optional algorithm by using hardware
   primitives. *)

module Binomial = struct
  let float_max f f' = if f < f' then f' else f

  let calc =
    let n = 31 in
    let up_pow = Array.make (2 * n + 1) 0. in
    let p = Array.make (n + 1) 0. in
    fun t s k r sigma q ->
      let delta_t = t /. (float_of_int n) in
      let up = exp (sigma *. (sqrt delta_t)) in
      let e = exp ((q -. r) *. delta_t) in
      let p0 = (up -. e) /. (up *. up -. 1.) in
      let p1 = e -. p0 in

      let upp = ref 1. in
      for i = 0 to n do
        up_pow.(n - i) <- 1. /. !upp;
        up_pow.(n + i) <- !upp;
        upp := !upp *. up
      done;

      for i = 0 to n do
        p.(i) <- float_max (k -. s *. up_pow.(2 * i)) 0.
      done;

      for j = n - 1 downto 0 do
        let nj = n - j in
        for i = 0 to j do
          p.(i) <- float_max (p0 *. p.(i + 1) +. p1 *. p.(i))
                             (k -. s *. up_pow.(2 * i + nj))
        done
      done;

      p.(0)

  let bench () =
    for i = 1 to 16 * 1024 do
      let _ = calc 2. 51. 52. 0.05 0.30 0. in ()
    done
end

type m128d
module M128d = struct
  module Array = struct
    type t
    type ptr

    let length t = Array.length (Obj.magic t) / 2
    external int_as_pointer : int -> ptr = "%int_as_pointer"
    external lsla : ptr -> int -> ptr = "%lsla"
    external adda : t -> ptr -> ptr = "%adda"
    let offset t i = adda t (lsla (int_as_pointer i) 3)
    external unsafe_get : ptr -> m128d = "%asm" "movapd	(%1), %0" "" ""
    external unsafe_set : ptr -> m128d -> unit = "%asm" "movapd	%2, (%1)" "" "" ""
    let unsafe_get t i = unsafe_get (offset t i)
    let unsafe_set t i v = unsafe_set (offset t i) v
    let get t i = unsafe_get t i
    let set t i e = unsafe_set t i e
    external make : int -> t = "caml_aligned_array_16_create"
    external create : int -> t = "caml_aligned_array_16_create"
    external free : t -> unit = "caml_aligned_array_16_free"
  end
  external ( +.. ) : m128d -> m128d -> m128d    = "%asm" "addpd	%2, %0" "0x" "mmx" ""
  external ( -.. ) : m128d -> m128d -> m128d    = "%asm" "subpd	%2, %0" "0" "mm" ""
  external ( *.. ) : m128d -> m128d -> m128d    = "%asm" "mulpd	%2, %0" "0x" "mmx" ""
  external ( /.. ) : m128d -> m128d -> m128d    = "%asm" "divpd	%2, %0" "0" "mm" ""
  external sqrt : m128d -> m128d            = "%asm" "sqrtpd	%1, %0" "mm" ""
  external min : m128d -> m128d -> m128d    = "%asm" "minpd	%2, %0" "0x" "mmx" ""
  external max : m128d -> m128d -> m128d    = "%asm" "maxpd	%2, %0" "0x" "mmx" ""
  external to_float : m128d -> float = "%asm" "movsd	%1, %0" "m" ""
  external setzero : unit -> m128d         = "%asm" "xorpd	%0, %0" "" ""
  external set : float -> float -> m128d  = "%asm" "unpcklpd	%1, %0" "mm" "0" ""
  let set1 x = set x x
  external unpackhi : m128d -> m128d -> m128d =
    "%asm" "unpckhpd	%0, %2" "0" "mm" ""
end

type m256d
module M256d = struct
  module Array = struct
    type t
    type ptr
    let length t = Array.length (Obj.magic t) / 4
    external int_as_pointer : int -> ptr = "%int_as_pointer"
    external lsla : ptr -> int -> ptr = "%lsla"
    external adda : t -> ptr -> ptr = "%adda"
    let offset t i = adda t (lsla (int_as_pointer i) 4)
    external unsafe_get : ptr -> m256d = "%asm" "vmovapd	(%1), %0" "" ""
    external unsafe_set : ptr -> m256d -> unit = "%asm" "vmovapd	%2, (%1)" "" "" ""
    let unsafe_get t i = unsafe_get (offset t i)
    let unsafe_set t i v = unsafe_set (offset t i) v
    let get t i = unsafe_get t i
    let set t i e = unsafe_set t i e
    external make : int -> t = "caml_aligned_array_32_create"
    external create : int -> t = "caml_aligned_array_32_create"
    external free : t -> unit = "caml_aligned_array_32_free"
  end
  external ( +.. ) : m256d -> m256d -> m256d    = "%asm" "vaddpd	%1, %2, %0" "x" "mmmx" ""
  external ( -.. ) : m256d -> m256d -> m256d    = "%asm" "vsubpd	%1, %2, %0" "" "mmm" ""
  external ( *.. ) : m256d -> m256d -> m256d    = "%asm" "vmulpd	%1, %2, %0" "x" "mmmx" ""
  external ( /.. ) : m256d -> m256d -> m256d    = "%asm" "vdivpd	%1, %2, %0" "" "mmm" ""
  external sqrt : m256d -> m256d            = "%asm" "vsqrtpd	%1, %0" "mmm" ""
  external min : m256d -> m256d -> m256d    = "%asm" "vminpd	%1, %2, %0" "x" "mmmx" ""
  external max : m256d -> m256d -> m256d    = "%asm" "vmaxpd	%1, %2, %0" "x" "mmmx" ""
  external to_float : m256d -> float = "%asm" "" "" ""
  external setzero : unit -> m256d         = "%asm" "vxorpd	%0, %0, %0" "" ""
  external unpackhi : m256d -> m256d -> m256d =
    "%asm" "vunpckhpd	%1, %2, %0" "mmm" "" ""
  external unpacklo : m256d -> m256d -> m256d =
    "%asm" "vunpcklpd	%1, %2, %0" "mmm" "" ""
  external of_m128 : m128d -> m256d = "%asm" "" "" ""
  external set : float -> float -> m128d  = "%asm" "vunpcklpd	%1, %2, %0" "mm" "" ""
  let set x y z w =
    let xy = set x y in
    let zw = set z w in
    unpacklo (of_m128 xy) (of_m128 zw)
  let set1 x = set x x x x
end

module Binomial_128 = struct
  let calc =
    let open M128d in
    let exp v =
      let x = to_float v in
      let y = to_float (unpackhi v v) in
      set (exp x) (exp y)
    in
    let n = 31 in
    let up_pow = Array.make (2 * n + 1) in
    let p = Array.make (n + 1) in
    fun t s k r sigma q ->
      let delta_t = t /.. (set (float_of_int n) (float_of_int n)) in
      let up = exp (sigma *.. (sqrt delta_t)) in
      let e = exp ((q -.. r) *.. delta_t) in
      let one = set1 1. in
      let p0 = (up -.. e) /.. (up *.. up -.. one) in
      let p1 = e -.. p0 in

      let upp = ref one in
      for i = 0 to n do
        Array.set up_pow (n - i) (one /.. !upp);
        Array.set up_pow (n + i) !upp;
        upp := !upp *.. up;
      done;

      let zero = setzero () in
      for i = 0 to n do
        Array.set p i (max (k -.. s *.. Array.get up_pow (2 * i)) zero)
      done;

      for j = n - 1 downto 0 do
        let nj = n - j in
        for i = 0 to j do
          Array.set p i (max (p0 *.. Array.get p (i + 1) +.. p1 *.. Array.get p i)
                         (k -.. s *.. Array.get up_pow (2 * i + nj)))
        done
      done;

      Array.get p 0

  let bench () =
    let open M128d in
    let ts = set 2. 2. in
    let s = set 51. 51. in
    let k = set 52. 52. in
    let r = set 0.05 0.05 in
    let sigma = set 0.30 0.30 in
    let q = set 0. 0. in
    for i = 1 to 8 * 1024 do
      let _ = calc ts s k r sigma q in ()
    done
end

module Binomial_256 = struct
  let calc =
    let open M256d in
    let exp v =
      let x = to_float v in
      let y = to_float (unpackhi v v) in
      set (exp x) (exp y) (exp x) (exp y)
    in
    let n = 31 in
    let up_pow = Array.make (2 * n + 1) in
    let p = Array.make (n + 1) in
    fun t s k r sigma q ->
      let nf = float_of_int n in
      let delta_t = t /.. (set nf nf nf nf) in
      let up = exp (sigma *.. (sqrt delta_t)) in
      let e = exp ((q -.. r) *.. delta_t) in
      let one = set1 1. in
      let p0 = (up -.. e) /.. (up *.. up -.. one) in
      let p1 = e -.. p0 in

      let upp = ref one in
      for i = 0 to n do
        Array.set up_pow (n - i) (one /.. !upp);
        Array.set up_pow (n + i) !upp;
        upp := !upp *.. up;
      done;

      let zero = setzero () in
      for i = 0 to n do
        Array.set p i (max (k -.. s *.. Array.get up_pow (2 * i)) zero)
      done;

      for j = n - 1 downto 0 do
        let nj = n - j in
        for i = 0 to j do
          Array.set p i (max (p0 *.. Array.get p (i + 1) +.. p1 *.. Array.get p i)
                         (k -.. s *.. Array.get up_pow (2 * i + nj)))
        done
      done;

      Array.get p 0

  let bench () =
    let open M256d in
    let ts = set 2. 2. 2. 2. in
    let s = set 51. 51. 51. 51. in
    let k = set 52. 52. 52. 52. in
    let r = set 0.05 0.05 0.05 0.05 in
    let sigma = set 0.30 0.30 0.30 0.30 in
    let q = set 0. 0. 0. 0. in
    for i = 1 to 4 * 1024 do
      let _ = calc ts s k r sigma q in ()
    done
end

let () =
  let t0 = Unix.gettimeofday () in
  Binomial.bench ();
  let t1 = Unix.gettimeofday () in
  Binomial_128.bench ();
  let t2 = Unix.gettimeofday () in
  Binomial_256.bench ();
  let t3 = Unix.gettimeofday () in
  let t3 = t3 -. t2 in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  Printf.printf "%f %f %f %f %f\n" t1 t2 t3 (t1 /. t2) (t1 /. t3)

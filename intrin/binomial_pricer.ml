(* The benchmark shows speedup of binomial optional algorithm by vectorizing it *)

module Cpuid = struct
  type t = {
    a : int;
    b : int;
    c : int;
    d : int }
  external __cpuid : int -> t = "ocaml___cpuid_stub"

  (* %ecx *)
  let bit_SSE3        = 1 lsl 0
  let bit_PCLMUL      = 1 lsl 1
  let bit_SSSE3       = 1 lsl 9
  let bit_FMA         = 1 lsl 12
  let bit_CMPXCHG16B  = 1 lsl 13
  let bit_SSE4_1      = 1 lsl 19
  let bit_SSE4_2      = 1 lsl 20
  let bit_MOVBE       = 1 lsl 22
  let bit_POPCNT      = 1 lsl 23
  let bit_AES         = 1 lsl 25
  let bit_XSAVE       = 1 lsl 26
  let bit_OSXSAVE     = 1 lsl 27
  let bit_AVX         = 1 lsl 28
  let bit_F16C        = 1 lsl 29
  let bit_RDRND       = 1 lsl 30

  (* %edx *)
  let bit_CMPXCHG8B   = 1 lsl 8
  let bit_CMOV        = 1 lsl 15
  let bit_MMX         = 1 lsl 23
  let bit_FXSAVE      = 1 lsl 24
  let bit_SSE         = 1 lsl 25
  let bit_SSE2        = 1 lsl 26

  (* Extended Features *)
  (* %ecx *)
  let bit_LAHF_LM     = 1 lsl 0
  let bit_ABM         = 1 lsl 5
  let bit_SSE4a       = 1 lsl 6
  let bit_XOP         = 1 lsl 11
  let bit_LWP         = 1 lsl 15
  let bit_FMA4        = 1 lsl 16
  let bit_TBM         = 1 lsl 21

  (* %edx *)
  let bit_LM          = 1 lsl 29
  let bit_3DNOWP      = 1 lsl 30
  let bit_3DNOW       = 1 lsl 31

  (* Extended Features (%eax == 7) *)
  let bit_FSGSBASE    = 1 lsl 0
  let bit_BMI         = 1 lsl 3
end

external ident : 'a -> 'a = "%identity"

type m128d
module M128d = struct
  module Array = struct
    type t

    let length t = Array.length (Obj.magic t) / 2
    external unsafe_get : t -> int -> m128d = "%asm" ""
       "movapd	-8(%0,%1,8), %2	# M128d.Array.unsafe_get" "r" "r" "=x"
    external unsafe_set : t -> int -> m128d -> unit = "%asm" ""
       "movapd	%2, -8(%0,%1,8)	# M128d.Array.unsafe_set" "r" "r" "x" ""
    let get t i = unsafe_get t i
    let set t i e = unsafe_set t i e
    external make : int -> t   = "caml_aligned_array_16_create"
    external create : int -> t = "caml_aligned_array_16_create"
    external free : t -> unit  = "caml_aligned_array_16_free"
  end
  external ( +.. ) : m128d -> m128d -> m128d  = "%asm" ""
       "addpd	%1, %2	# M128d.(+..)" "%2" "xm128" "=x"
  external ( -.. ) : m128d -> m128d -> m128d  = "%asm" ""
       "subpd	%1, %2	# M128d.(-..)" "2" "xm128" "=x"
  external ( *.. ) : m128d -> m128d -> m128d  = "%asm" ""
       "mulpd	%1, %2	# M128d.(*..)" "%2" "xm128" "=x"
  external ( /.. ) : m128d -> m128d -> m128d  = "%asm" ""
       "divpd	%1, %2	# M128d.(/..)" "2" "xm128" "=x"
  external sqrt : m128d -> m128d              = "%asm" ""
       "sqrtpd	%0, %1	# M128d.sqrt" "xm128" "=x"
  external min : m128d -> m128d -> m128d      = "%asm" ""
       "minpd	%1, %2	# M128d.min" "%2" "xm128" "=x"
  external max : m128d -> m128d -> m128d      = "%asm" ""
       "maxpd	%1, %2	# M128d.max" "%2" "xm128" "=x"
  external of_float : float -> m128d          = "%asm" ""
       "movapd	%0, %1	# M128d.of_float" "x" "=x"
  external to_float : m128d -> float          = "%asm" ""
       "movapd	%0, %1	# M128d.to_float" "x" "=x"
  external setzero : unit -> m128d            = "%asm" ""
       "xorpd	%1, %1	# M128d.setzero" "" "=x"
  external unpacklo : m128d -> m128d -> m128d = "%asm" ""
       "unpcklpd	%1, %2	# M128d.unpacklo" "2" "xm128" "=x"
  external unpackhi : m128d -> m128d -> m128d = "%asm" ""
       "unpckhpd	%1, %2	# M128d.unpackhi" "2" "xm128" "=x"
  let set x y = unpacklo (of_float x) (of_float y)
  let set1 x = set x x
end

type m256d
module M256d = struct
  module Array = struct
    type t
    let length t = Array.length (Obj.magic t) / 4
    external unsafe_get : t -> int -> m256d = "%asm" ""
       "vmovapd	-8(%0,%1,8), %2	# M256d.Array.unsafe_get" "r" "r" "=x"
    external unsafe_set : t -> int -> m256d -> unit = "%asm" ""
       "vmovapd	%2, -8(%0,%1,8)	# M256d.Array.unsafe_set" "r" "r" "x" ""
    let unsafe_get t i   = unsafe_get t (i * 2)
    let unsafe_set t i v = unsafe_set t (i * 2) v
    let get t i = unsafe_get t i
    let set t i e = unsafe_set t i e
    external make : int -> t   = "caml_aligned_array_32_create"
    external create : int -> t = "caml_aligned_array_32_create"
    external free : t -> unit  = "caml_aligned_array_32_free"
  end
  external ( +.. ) : m256d -> m256d -> m256d  = "%asm" ""
       "vaddpd	%0, %1, %2	# M256d.(+..)" "%x" "xm256" "=x"
  external ( -.. ) : m256d -> m256d -> m256d  = "%asm" ""
       "vsubpd	%1, %0, %2	# M256d.(-..)" "x" "xm256" "=x"
  external ( *.. ) : m256d -> m256d -> m256d  = "%asm" ""
       "vmulpd	%0, %1, %2	# M256d.(*..)" "%x" "xm256" "=x"
  external ( /.. ) : m256d -> m256d -> m256d  = "%asm" ""
       "vdivpd	%1, %0, %2	# M256d.(/..)" "x" "xm256" "=x"
  external sqrt : m256d -> m256d              = "%asm" ""
       "vsqrtpd	%0, %1	# M256.sqrt" "xm256" "=x"
  external min : m256d -> m256d -> m256d      = "%asm" ""
       "vminpd	%0, %1, %2	# M256d.min" "%x" "xm256" "=x"
  external max : m256d -> m256d -> m256d      = "%asm" ""
       "vmaxpd	%0, %1, %2	# M256d.max" "%x" "xm256" "=x"
  external setzero : unit -> m256d            = "%asm" ""
       "vxorpd	%1, %1, %1	# M256d.setzero" "" "=x"
  external unpackhi : m256d -> m256d -> m256d = "%asm" ""
       "vunpckhpd	%0, %1, %2	# M256d.unpackhi" "x" "xm256" "=x"
  external unpacklo : m256d -> m256d -> m256d = "%asm" ""
       "vunpcklpd	%0, %1, %2	# M256d.unpacklo" "x" "xm256" "=x"
  external of_m128 : m128d -> m256d           = "%asm" ""
       "movapd	%x0, %x1		# M256d.of_m128" "x" "=x"
  external to_m128 : m256d -> m128d           = "%asm" ""
       "movapd	%x0, %x1		# M256d.to_m128" "x" "=x"
  let set x y z w =
    unpacklo (of_m128 (M128d.set x y)) (of_m128 (M128d.set z w))
  let set1 x = set x x x x
end

module Binomial = struct
  let float_max (f : float) (f' : float) = if f < f' then f' else f

  let calc =
    let n = 31 in
    let up_pow = Array.make (2 * n + 1) 0. in
    let p = Array.make (n + 1) 0. in
    fun t s k r sigma q ->
      let delta_t = t /. (float_of_int n) in
      let up = exp (sigma *. (sqrt delta_t)) in
      let e = exp ((q -. r) *. delta_t) in

      let up_pow = ident up_pow in
      let p = ident p in

      let p0 = (up -. e) /. (up *. up -. 1.) in
      let p1 = e -. p0 in

      let upp = ref 1. in
      for i = 0 to n do
        Array.unsafe_set up_pow (n - i) (1. /. !upp);
        Array.unsafe_set up_pow (n + i) !upp;
        upp := !upp *. up
      done;

      let s = s +. 0. in
      let k = k +. 0. in
      for i = 0 to n do
        Array.unsafe_set p i (float_max (k -. s *. Array.unsafe_get up_pow (2 * i)) 0.)
      done;

      for j = n - 1 downto 0 do
        let nj = n - j in
        for i = 0 to j do
          Array.unsafe_set p i (float_max
            (p0 *. Array.unsafe_get p (i + 1) +. p1 *. Array.unsafe_get p i)
            (k -. s *. Array.unsafe_get up_pow (2 * i + nj)))
        done
      done;

      Array.unsafe_get p 0

  let bench () =
    for i = 1 to 16 * 1024 do
      let x = calc 2. 51. 52. 0.05 0.30 0. in
      assert (x = 7.098116295410293652423661)
    done
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
      let nf = float_of_int n in
      let delta_t = t /.. (set nf nf) in
      let up = exp (sigma *.. (sqrt delta_t)) in
      let e = exp ((q -.. r) *.. delta_t) in

      let up_pow = ident up_pow in
      let p = ident p in

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
      let s = s +.. zero in
      let k = k +.. zero in
      for i = 0 to n do
        Array.set p i (max (k -.. s *.. Array.get up_pow (2 * i)) zero)
      done;

      for j = n - 1 downto 0 do
        let nj = n - j in
        for i = 0 to j do
          Array.set p i (max (p0 *.. Array.get p (i + 1) +.. p1 *.. Array.get p i)
                             (k -.. s *.. Array.get up_pow (i * 2 + nj)))
        done
      done;

      Array.get p 0

  let bench () =
    let open M128d in
    let t = set 2. 2. in
    let s = set 51. 51. in
    let k = set 52. 52. in
    let r = set 0.05 0.05 in
    let sigma = set 0.30 0.30 in
    let q = set 0. 0. in
    for i = 1 to 8 * 1024 do
      let x = calc t s k r sigma q in
      assert (to_float x = 7.098116295410293652423661)
    done
end

module Binomial_256 = struct
  let calc =
    let open M256d in
    let exp v =
      let xy = to_m128 v in
      let zw = to_m128 (unpackhi v v) in
      let x = M128d.to_float xy in
      let y = M128d.to_float (M128d.unpackhi xy xy) in
      let z = M128d.to_float zw in
      let w = M128d.to_float (M128d.unpackhi zw zw) in
      set (exp x) (exp y) (exp z) (exp w)
    in
    let n = 31 in
    let up_pow = Array.make (2 * n + 1) in
    let p = Array.make (n + 1) in
    fun t s k r sigma q ->
      let nf = float_of_int n in
      let nf = set nf nf nf nf in
      let delta_t = t /.. nf in
      let up = exp (sigma *.. (sqrt delta_t)) in
      let e = exp ((q -.. r) *.. delta_t) in

      let up_pow = ident up_pow in
      let p = ident p in

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
      let s = s +.. zero in
      let k = k +.. zero in
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
    let t = set 2. 2. 2. 2. in
    let s = set 51. 51. 51. 51. in
    let k = set 52. 52. 52. 52. in
    let r = set 0.05 0.05 0.05 0.05 in
    let sigma = set 0.30 0.30 0.30 0.30 in
    let q = set 0. 0. 0. 0. in
    for i = 1 to 4 * 1024 do
      let x = calc t s k r sigma q in
      assert (M128d.to_float (to_m128 x) = 7.098116295410293652423661)
    done
end

let () =
  let supports_avx =
    let open Cpuid in
    let cpuid = __cpuid 1 in
    cpuid.c land bit_AVX <> 0
  in
  let t0 = Unix.gettimeofday () in
  Binomial.bench ();
  let t1 = Unix.gettimeofday () in
  Binomial_128.bench ();
  let t2 = Unix.gettimeofday () in
  if supports_avx then
    Binomial_256.bench ();
  let t3 = Unix.gettimeofday () in
  let t3 = t3 -. t2 in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  let ns_mult = 1_000_000_000. /. float_of_int (16 * 1024) in
  Printf.printf "SSE2 : %f ns vs %f ns, speedup %f\n"
    (t1 *. ns_mult) (t2 *. ns_mult) (t1 /. t2);
  if supports_avx then
    Printf.printf "AVX  : %f ns vs %f ns, speedup %f\n"
      (t1 *. ns_mult) (t3 *. ns_mult) (t1 /. t3);

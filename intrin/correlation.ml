let correlation_matrix_dumb ~n ~stride xs c =
  for s = 0 to 3 do
    let s = s * stride in
    for i = 0 to n - 1 do
      let i_stride = i * stride in
      let x = Array.unsafe_get xs (s + i) in
      for j = 0 to n - 1 do
        let y = Array.unsafe_get xs (s + j) in
        let i_n_j = i_stride + j in
        Array.unsafe_set c i_n_j (Array.unsafe_get c i_n_j +. x *. y)
      done
    done
  done

let correlation_matrix_opt ~n ~stride xs c =
  let stride_vec = stride / 1 in
  let i = ref 0 in
  while !i < n do
    let a0 = Array.unsafe_get xs (stride * 0 + !i + 0) in
    let a1 = Array.unsafe_get xs (stride * 1 + !i + 0) in
    let a2 = Array.unsafe_get xs (stride * 2 + !i + 0) in
    let a3 = Array.unsafe_get xs (stride * 3 + !i + 0) in
    let b0 = Array.unsafe_get xs (stride * 0 + !i + 1) in
    let b1 = Array.unsafe_get xs (stride * 1 + !i + 1) in
    let b2 = Array.unsafe_get xs (stride * 2 + !i + 1) in
    let b3 = Array.unsafe_get xs (stride * 3 + !i + 1) in
    let c0 = Array.unsafe_get xs (stride * 0 + !i + 2) in
    let c1 = Array.unsafe_get xs (stride * 1 + !i + 2) in
    let c2 = Array.unsafe_get xs (stride * 2 + !i + 2) in
    let c3 = Array.unsafe_get xs (stride * 3 + !i + 2) in
    let sti = !i * stride_vec in
    let j_end = (!i + 2) / 1 in
    let i0 = ref (sti + stride_vec * 0) in
    let i1 = ref (sti + stride_vec * 1) in
    let i2 = ref (sti + stride_vec * 2) in
    let st0 = ref (stride_vec * 0) in
    let st1 = ref (stride_vec * 1) in
    let st2 = ref (stride_vec * 2) in
    let st3 = ref (stride_vec * 3) in
    while !st0 <= j_end do
      let s0 = Array.unsafe_get c !i0 in
      let s1 = Array.unsafe_get c !i1 in
      let s2 = Array.unsafe_get c !i2 in
      let xj = Array.unsafe_get xs !st0 in
      st0 := !st0 + 1;
      let s0 = s0 +. a0 *. xj in
      let s1 = s1 +. b0 *. xj in
      let s2 = s2 +. c0 *. xj in
      let xj = Array.unsafe_get xs !st1 in
      st1 := !st1 + 1;
      let s0 = s0 +. a1 *. xj in
      let s1 = s1 +. b1 *. xj in
      let s2 = s2 +. c1 *. xj in
      let xj = Array.unsafe_get xs !st2 in
      st2 := !st2 + 1;
      let s0 = s0 +. a2 *. xj in
      let s1 = s1 +. b2 *. xj in
      let s2 = s2 +. c2 *. xj in
      let xj = Array.unsafe_get xs !st3 in
      st3 := !st3 + 1;
      let s0 = s0 +. a3 *. xj in
      let s1 = s1 +. b3 *. xj in
      let s2 = s2 +. c3 *. xj in
      Array.unsafe_set c !i0 s0;
      i0 := !i0 + 1;
      Array.unsafe_set c !i1 s1;
      i1 := !i1 + 1;
      Array.unsafe_set c !i2 s2;
      i2 := !i2 + 1;
    done;
    i := !i + 3
  done

type m128d

external _mm_loadu_pd : float array -> int -> m128d = "%asm" ""
     "movupd	-8(%0,%1,8), %2	# _mm_loadu_pd" "r" "r" "=x"
external _mm_storeu_pd : float array -> int -> m128d -> unit = "%asm" ""
     "movupd	%2, -8(%0,%1,8)	# _mm_storeu_pd" "r" "r" "x" "r"
external _mm_add_pd : m128d -> m128d -> m128d = "%asm" ""
     "addpd	%1, %2	# _mm_add_pd" "%2" "xm128" "=x"
external _mm_mul_pd : m128d -> m128d -> m128d = "%asm" ""
     "mulpd	%1, %2	# _mm_mul_pd" "%2" "xm128" "=x"
external _mm_set1_pd : float -> m128d = "%asm" ""
     "unpcklpd	%1, %1	# _mm_set1_pd" "1" "=x"

let correlation_matrix_sse ~n ~stride xs c =
  let stride_vec = stride / 2 in
  let i = ref 0 in
  while !i < n do
    let a0 = _mm_set1_pd (Array.unsafe_get xs (stride * 0 + !i + 0)) in
    let a1 = _mm_set1_pd (Array.unsafe_get xs (stride * 1 + !i + 0)) in
    let a2 = _mm_set1_pd (Array.unsafe_get xs (stride * 2 + !i + 0)) in
    let a3 = _mm_set1_pd (Array.unsafe_get xs (stride * 3 + !i + 0)) in
    let b0 = _mm_set1_pd (Array.unsafe_get xs (stride * 0 + !i + 1)) in
    let b1 = _mm_set1_pd (Array.unsafe_get xs (stride * 1 + !i + 1)) in
    let b2 = _mm_set1_pd (Array.unsafe_get xs (stride * 2 + !i + 1)) in
    let b3 = _mm_set1_pd (Array.unsafe_get xs (stride * 3 + !i + 1)) in
    let c0 = _mm_set1_pd (Array.unsafe_get xs (stride * 0 + !i + 2)) in
    let c1 = _mm_set1_pd (Array.unsafe_get xs (stride * 1 + !i + 2)) in
    let c2 = _mm_set1_pd (Array.unsafe_get xs (stride * 2 + !i + 2)) in
    let c3 = _mm_set1_pd (Array.unsafe_get xs (stride * 3 + !i + 2)) in
    let sti = !i * stride_vec in
    let ( +. ) = _mm_add_pd in
    let ( *. ) = _mm_mul_pd in
    let j_end = (!i + 2) / 2 in
    let i0 = ref (sti + stride_vec * 0) in
    let i1 = ref (sti + stride_vec * 1) in
    let i2 = ref (sti + stride_vec * 2) in
    let st0 = ref (stride_vec * 0) in
    let st1 = ref (stride_vec * 1) in
    let st2 = ref (stride_vec * 2) in
    let st3 = ref (stride_vec * 3) in
    while !st0 <= j_end do
      let s0 = _mm_loadu_pd c !i0 in
      let s1 = _mm_loadu_pd c !i1 in
      let s2 = _mm_loadu_pd c !i2 in
      let xj = _mm_loadu_pd xs !st0 in
      st0 := !st0 + 1;
      let s0 = s0 +. a0 *. xj in
      let s1 = s1 +. b0 *. xj in
      let s2 = s2 +. c0 *. xj in
      let xj = _mm_loadu_pd xs !st1 in
      st1 := !st1 + 1;
      let s0 = s0 +. a1 *. xj in
      let s1 = s1 +. b1 *. xj in
      let s2 = s2 +. c1 *. xj in
      let xj = _mm_loadu_pd xs !st2 in
      st2 := !st2 + 1;
      let s0 = s0 +. a2 *. xj in
      let s1 = s1 +. b2 *. xj in
      let s2 = s2 +. c2 *. xj in
      let xj = _mm_loadu_pd xs !st3 in
      st3 := !st3 + 1;
      let s0 = s0 +. a3 *. xj in
      let s1 = s1 +. b3 *. xj in
      let s2 = s2 +. c3 *. xj in
      _mm_storeu_pd c !i0 s0;
      i0 := !i0 + 1;
      _mm_storeu_pd c !i1 s1;
      i1 := !i1 + 1;
      _mm_storeu_pd c !i2 s2;
      i2 := !i2 + 1;
    done;
    i := !i + 3
  done

type m256d

external _mm256_loadu_pd : float array -> int -> m256d = "%asm" ""
     "vmovupd	-8(%0,%1,8), %2	# _mm256_loadu_pd" "r" "r" "=x"
external _mm256_storeu_pd : float array -> int -> m256d -> unit = "%asm" ""
     "vmovupd	%2, -8(%0,%1,8)	# _mm256_storeu_pd" "r" "r" "x" "r"
external _mm256_add_pd : m256d -> m256d -> m256d = "%asm" ""
     "vaddpd	%1, %0, %2	# _mm256_add_pd" "%x" "xm256" "=x"
external _mm256_mul_pd : m256d -> m256d -> m256d = "%asm" ""
     "vmulpd	%1, %0, %2	# _mm256_mul_pd" "%x" "xm256" "=x"
external _mm256_set1_pd : float -> m256d = "%asm" ""
     "vmovddup	%x0, %x1	# _mm256_set1_pd
	vinsertf128	$1, %x1, %1, %1" "xm64" "=x"

let correlation_matrix_avx ~n ~stride xs c =
  let stride_vec = stride / 4 * 2 in
  let i = ref 0 in
  while !i < n do
    let a0 = _mm256_set1_pd (Array.unsafe_get xs (stride * 0 + !i + 0)) in
    let a1 = _mm256_set1_pd (Array.unsafe_get xs (stride * 1 + !i + 0)) in
    let a2 = _mm256_set1_pd (Array.unsafe_get xs (stride * 2 + !i + 0)) in
    let a3 = _mm256_set1_pd (Array.unsafe_get xs (stride * 3 + !i + 0)) in
    let b0 = _mm256_set1_pd (Array.unsafe_get xs (stride * 0 + !i + 1)) in
    let b1 = _mm256_set1_pd (Array.unsafe_get xs (stride * 1 + !i + 1)) in
    let b2 = _mm256_set1_pd (Array.unsafe_get xs (stride * 2 + !i + 1)) in
    let b3 = _mm256_set1_pd (Array.unsafe_get xs (stride * 3 + !i + 1)) in
    let c0 = _mm256_set1_pd (Array.unsafe_get xs (stride * 0 + !i + 2)) in
    let c1 = _mm256_set1_pd (Array.unsafe_get xs (stride * 1 + !i + 2)) in
    let c2 = _mm256_set1_pd (Array.unsafe_get xs (stride * 2 + !i + 2)) in
    let c3 = _mm256_set1_pd (Array.unsafe_get xs (stride * 3 + !i + 2)) in
    let sti = !i * stride_vec in
    let ( +. ) = _mm256_add_pd in
    let ( *. ) = _mm256_mul_pd in
    let j_end = (!i + 2) / 4 * 2 in
    let i0 = ref (sti + stride_vec * 0) in
    let i1 = ref (sti + stride_vec * 1) in
    let i2 = ref (sti + stride_vec * 2) in
    let st0 = ref (stride_vec * 0) in
    let st1 = ref (stride_vec * 1) in
    let st2 = ref (stride_vec * 2) in
    let st3 = ref (stride_vec * 3) in
    while !st0 <= j_end do
      let s0 = _mm256_loadu_pd c !i0 in
      let s1 = _mm256_loadu_pd c !i1 in
      let s2 = _mm256_loadu_pd c !i2 in
      let xj = _mm256_loadu_pd xs !st0 in
      st0 := !st0 + 2;
      let s0 = s0 +. a0 *. xj in
      let s1 = s1 +. b0 *. xj in
      let s2 = s2 +. c0 *. xj in
      let xj = _mm256_loadu_pd xs !st1 in
      st1 := !st1 + 2;
      let s0 = s0 +. a1 *. xj in
      let s1 = s1 +. b1 *. xj in
      let s2 = s2 +. c1 *. xj in
      let xj = _mm256_loadu_pd xs !st2 in
      st2 := !st2 + 2;
      let s0 = s0 +. a2 *. xj in
      let s1 = s1 +. b2 *. xj in
      let s2 = s2 +. c2 *. xj in
      let xj = _mm256_loadu_pd xs !st3 in
      st3 := !st3 + 2;
      let s0 = s0 +. a3 *. xj in
      let s1 = s1 +. b3 *. xj in
      let s2 = s2 +. c3 *. xj in
      _mm256_storeu_pd c !i0 s0;
      i0 := !i0 + 2;
      _mm256_storeu_pd c !i1 s1;
      i1 := !i1 + 2;
      _mm256_storeu_pd c !i2 s2;
      i2 := !i2 + 2;
    done;
    i := !i + 3
  done

let () =
  let n = 256 in
  let stride = (n + 11) / 12 * 12 in
  let samples = 4 in
  let iters = 1024 * 4 in
  let c = Array.make (stride * stride) 0. in
  let xs = Array.init (stride * samples) (fun _ -> Random.float 2. -. 1.) in
  let c0 = Array.make (stride * stride) 0. in
  let c1 = Array.make (stride * stride) 0. in
  let c2 = Array.make (stride * stride) 0. in
  let c3 = Array.make (stride * stride) 0. in
  correlation_matrix_dumb ~n ~stride xs c0;
  correlation_matrix_opt  ~n ~stride xs c1;
  correlation_matrix_sse  ~n ~stride xs c2;
  correlation_matrix_avx  ~n ~stride xs c3;
  for i = 0 to n - 1 do
    for j = 0 to i do
      let x0 = c0.(i * stride + j) in
      let x1 = c1.(i * stride + j) in
      let x2 = c2.(i * stride + j) in
      let x3 = c3.(i * stride + j) in
      assert (x0 = x1);
      assert (x0 = x2);
      assert (x0 = x3);
    done
  done;
  let t0 = Unix.gettimeofday () in
  for i = 0 to iters - 1 do
    correlation_matrix_opt ~n ~stride xs c;
  done;
  let t1 = Unix.gettimeofday () in
  for i = 0 to iters - 1 do
    correlation_matrix_sse ~n ~stride xs c;
  done;
  let t2 = Unix.gettimeofday () in
  for i = 0 to iters - 1 do
    correlation_matrix_avx ~n ~stride xs c;
  done;
  let t3 = Unix.gettimeofday () in
  let t3 = t3 -. t2 in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  Printf.printf "Scalar: %f s\n" t1;
  Printf.printf "SSE   : %f s\n" t2;
  Printf.printf "AVX   : %f s\n" t3

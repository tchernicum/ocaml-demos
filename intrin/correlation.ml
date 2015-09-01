let correlation_matrix ~n ~samples xs c =
  for s = 0 to samples - 1 do
    let s = s * n in
    for i = 0 to n - 1 do
      let i_n = i * n in
      let x = Array.unsafe_get xs (s + i) in
      for j = 0 to i do
        let y = Array.unsafe_get xs (s + j) in
        let i_n_j = i_n + j in
        Array.unsafe_set c i_n_j (Array.unsafe_get c i_n_j +. x *. y)
      done
    done
  done

let correlation_matrix_opt ~n ~stride ~samples xs c =
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
    for j = 0 to (!i + 1) / 1 do
      let st0 = stride_vec * 0 + j in
      let i0 = sti + st0 in
      let st1 = stride_vec * 1 + j in
      let i1 = sti + st1 in
      let st2 = stride_vec * 2 + j in
      let i2 = sti + st2 in
      let st3 = stride_vec * 3 + j in
      let s0 = Array.unsafe_get c i0 in
      let s1 = Array.unsafe_get c i1 in
      let s2 = Array.unsafe_get c i2 in
      let xj = Array.unsafe_get xs st0 in
      let s0 = s0 +. a0 *. xj in
      let s1 = s1 +. b0 *. xj in
      let s2 = s2 +. c0 *. xj in
      let xj = Array.unsafe_get xs st1 in
      let s0 = s0 +. a1 *. xj in
      let s1 = s1 +. b1 *. xj in
      let s2 = s2 +. c1 *. xj in
      let xj = Array.unsafe_get xs st2 in
      let s0 = s0 +. a2 *. xj in
      let s1 = s1 +. b2 *. xj in
      let s2 = s2 +. c2 *. xj in
      let xj = Array.unsafe_get xs st3 in
      let s0 = s0 +. a3 *. xj in
      let s1 = s1 +. b3 *. xj in
      let s2 = s2 +. c3 *. xj in
      Array.unsafe_set c i0 s0;
      Array.unsafe_set c i1 s1;
      Array.unsafe_set c i2 s2
    done;
    i := !i + 3
  done

type m128d

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
external _mm_set1_pd : float -> m128d = "%asm" ""
     "unpcklpd	%1, %1	# _mm_set1_pd" "1" "=x"

let correlation_matrix_sse ~n ~stride ~samples xs c =
  let stride_vec = stride / 2 in
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
    for j = 0 to (!i + 1) / 1 do
      let st0 = stride_vec * 0 + j in
      let i0 = sti + st0 in
      let st1 = stride_vec * 1 + j in
      let i1 = sti + st1 in
      let st2 = stride_vec * 2 + j in
      let i2 = sti + st2 in
      let st3 = stride_vec * 3 + j in
      let s0 = Array.unsafe_get c i0 in
      let s1 = Array.unsafe_get c i1 in
      let s2 = Array.unsafe_get c i2 in
      let xj = Array.unsafe_get xs st0 in
      let s0 = s0 +. a0 *. xj in
      let s1 = s1 +. b0 *. xj in
      let s2 = s2 +. c0 *. xj in
      let xj = Array.unsafe_get xs st1 in
      let s0 = s0 +. a1 *. xj in
      let s1 = s1 +. b1 *. xj in
      let s2 = s2 +. c1 *. xj in
      let xj = Array.unsafe_get xs st2 in
      let s0 = s0 +. a2 *. xj in
      let s1 = s1 +. b2 *. xj in
      let s2 = s2 +. c2 *. xj in
      let xj = Array.unsafe_get xs st3 in
      let s0 = s0 +. a3 *. xj in
      let s1 = s1 +. b3 *. xj in
      let s2 = s2 +. c3 *. xj in
      Array.unsafe_set c i0 s0;
      Array.unsafe_set c i1 s1;
      Array.unsafe_set c i2 s2
    done;
    i := !i + 3
  done

let () =
  let n = 256 in
  let stride = (n + 2) / 3 * 3 in
  let samples = 4 in
  let iters = 1024 in
  let c = Array.make (n * stride) 0. in
  let xs = Array.init (stride * samples) (fun _ -> Random.float 2. -. 1.) in
  let t0 = Unix.gettimeofday () in
  for i = 0 to iters - 1 do
    correlation_matrix ~n ~samples xs c;
  done;
  let t1 = Unix.gettimeofday () in
  for i = 0 to iters - 1 do
    correlation_matrix_opt ~n ~stride ~samples xs c;
  done;
  let t2 = Unix.gettimeofday () in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  Printf.printf "%f %f\n%!" t1 t2

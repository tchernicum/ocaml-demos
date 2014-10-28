external float_compare_old : float -> float -> int = "caml_float_compare_old"

let () =
  let a = Array.init 100 (fun _ -> Random.float 100. -. 50.) in
  List.iteri
    (fun i x -> a.(i) <- x)
    [0.; 1.; -1.; nan; neg_infinity; infinity; max_float; -. max_float;
     2. ** -1074.; -. 2. ** -1074.];
  Array.iter (fun x ->
    Array.iter (fun y ->
      assert (compare x y = float_compare_old x y)) a) a

let a = Array.init 1_000_001 (fun _ -> Random.float 100.)
let b = Array.init 1_000_001 float_of_int

let bench (name : string) f =
  let count = 100 in
  let mw0, _, _ = Gc.counters () in
  let t0 = Unix.gettimeofday () in
  for i = 1 to count do
    f ()
  done;
  let t1 = Unix.gettimeofday () in
  let mw1, _, _ = Gc.counters () in
  let count = float_of_int count in
  Printf.printf "%f %9.0f %s\n" ((t1 -. t0) /. count) ((mw1 -. mw0) /. count) name;;

let () =
  bench "compare base" (fun () ->
    let a = a in (* Make the reference local, because it's in a tight loop *)
    let s = ref 0 in
    for i = 0 to Array.length a - 2 do
      let _x = Array.unsafe_get a i in
      let _y = Array.unsafe_get a (i + 1) in
      s := !s + 1
    done);
  bench "compare_old unpredictable x 1M" (fun () ->
    let a = a in
    let s = ref 0 in
    for i = 0 to Array.length a - 2 do
      let x = Array.unsafe_get a i in
      let y = Array.unsafe_get a (i + 1) in
      s := !s + float_compare_old x y
    done);
  bench "compare_old   predictable x 1M" (fun () ->
    let b = b in
    let s = ref 0 in
    for i = 0 to Array.length b - 2 do
      let x = Array.unsafe_get b i in
      let y = Array.unsafe_get b (i + 1) in
      s := !s + float_compare_old x y
    done);
  bench "compare     unpredictable x 1M" (fun () ->
    let a = a in
    let s = ref 0 in
    for i = 0 to Array.length a - 2 do
      let x = Array.unsafe_get a i in
      let y = Array.unsafe_get a (i + 1) in
      s := !s + compare x y
    done);
  bench "compare       predictable x 1M" (fun () ->
    let b = b in
    let s = ref 0 in
    for i = 0 to Array.length b - 2 do
      let x = Array.unsafe_get b i in
      let y = Array.unsafe_get b (i + 1) in
      s := !s + compare x y
    done)

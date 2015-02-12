open Bigarray

(* Benchmarks the speed of accessing and writing to Bigarray.t *)

let () =
  let a = Array1.create int8_signed c_layout (10 * 10000) in
  Array1.fill a 1;
  for i = 0 to 9999 do
    let i = i * 10 in
    assert (Array1.unsafe_get a (i + 0) = 1);
    assert (Array1.unsafe_get a (i + 1) = 1);
    assert (Array1.unsafe_get a (i + 2) = 1);
    assert (Array1.unsafe_get a (i + 3) = 1);
    assert (Array1.unsafe_get a (i + 4) = 1);
    assert (Array1.unsafe_get a (i + 5) = 1);
    assert (Array1.unsafe_get a (i + 6) = 1);
    assert (Array1.unsafe_get a (i + 7) = 1);
    assert (Array1.unsafe_get a (i + 8) = 1);
    assert (Array1.unsafe_get a (i + 9) = 1)
  done

let () =
  let t0 = Unix.gettimeofday () in
  let a = Array1.create float32 c_layout (10 * 10000) in
  let s = ref 0. in
  for i = 0 to 9999 do
    let i = i * 10 in
    let s0 = ref 0. in
    let s1 = ref 0. in
    let s2 = ref 0. in
    let s3 = ref 0. in
    let s4 = ref 0. in
    let s5 = ref 0. in
    let s6 = ref 0. in
    let s7 = ref 0. in
    let s8 = ref 0. in
    let s9 = ref 0. in
    for j = 0 to 9999 do
      s0 := Array1.unsafe_get a (i + 0);
      s1 := Array1.unsafe_get a (i + 1);
      s2 := Array1.unsafe_get a (i + 2);
      s3 := Array1.unsafe_get a (i + 3);
      s4 := Array1.unsafe_get a (i + 4);
      s5 := Array1.unsafe_get a (i + 5);
      s6 := Array1.unsafe_get a (i + 6);
      s7 := Array1.unsafe_get a (i + 7);
      s8 := Array1.unsafe_get a (i + 8);
      s9 := Array1.unsafe_get a (i + 9)
    done;
    s := !s +. !s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7 +. !s8 +. !s9
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %f\n%!" "float32 get" (t1 -. t0) (!s +. 0.);

  let t0 = Unix.gettimeofday () in
  let a = Array1.create float32 c_layout (10 * 10000) in
  for i = 0 to 9999 do
    let i = i * 10 in
    let x = 0. in
    for j = 0 to 9999 do
      Array1.unsafe_set a (i + 0) x;
      Array1.unsafe_set a (i + 1) x;
      Array1.unsafe_set a (i + 2) x;
      Array1.unsafe_set a (i + 3) x;
      Array1.unsafe_set a (i + 4) x;
      Array1.unsafe_set a (i + 5) x;
      Array1.unsafe_set a (i + 6) x;
      Array1.unsafe_set a (i + 7) x;
      Array1.unsafe_set a (i + 8) x;
      Array1.unsafe_set a (i + 9) x
    done;
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %f\n%!" "float32 set" (t1 -. t0) (!s +. 0.);

  let t0 = Unix.gettimeofday () in
  let a = Array1.create float64 c_layout (10 * 10000) in
  let s = ref 0. in
  for i = 0 to 9999 do
    let i = i * 10 in
    let s0 = ref 0. in
    let s1 = ref 0. in
    let s2 = ref 0. in
    let s3 = ref 0. in
    let s4 = ref 0. in
    let s5 = ref 0. in
    let s6 = ref 0. in
    let s7 = ref 0. in
    let s8 = ref 0. in
    let s9 = ref 0. in
    for j = 0 to 9999 do
      s0 := Array1.unsafe_get a (i + 0);
      s1 := Array1.unsafe_get a (i + 1);
      s2 := Array1.unsafe_get a (i + 2);
      s3 := Array1.unsafe_get a (i + 3);
      s4 := Array1.unsafe_get a (i + 4);
      s5 := Array1.unsafe_get a (i + 5);
      s6 := Array1.unsafe_get a (i + 6);
      s7 := Array1.unsafe_get a (i + 7);
      s8 := Array1.unsafe_get a (i + 8);
      s9 := Array1.unsafe_get a (i + 9)
    done;
    s := !s +. !s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7 +. !s8 +. !s9
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %f\n%!" "float64 get" (t1 -. t0) (!s +. 0.);

  let t0 = Unix.gettimeofday () in
  let a = Array1.create float64 c_layout (10 * 10000) in
  for i = 0 to 9999 do
    let i = i * 10 in
    let x = 0. in
    for j = 0 to 9999 do
      Array1.unsafe_set a (i + 0) x;
      Array1.unsafe_set a (i + 1) x;
      Array1.unsafe_set a (i + 2) x;
      Array1.unsafe_set a (i + 3) x;
      Array1.unsafe_set a (i + 4) x;
      Array1.unsafe_set a (i + 5) x;
      Array1.unsafe_set a (i + 6) x;
      Array1.unsafe_set a (i + 7) x;
      Array1.unsafe_set a (i + 8) x;
      Array1.unsafe_set a (i + 9) x
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %f\n%!" "float64 set" (t1 -. t0) (!s +. 0.);

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int8_signed c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let s0 = ref 0 in
    let s1 = ref 0 in
    let s2 = ref 0 in
    let s3 = ref 0 in
    let s4 = ref 0 in
    let s5 = ref 0 in
    let s6 = ref 0 in
    let s7 = ref 0 in
    let s8 = ref 0 in
    let s9 = ref 0 in
    for j = 0 to 9999 do
      s0 := Array1.unsafe_get a (i + 0);
      s1 := Array1.unsafe_get a (i + 1);
      s2 := Array1.unsafe_get a (i + 2);
      s3 := Array1.unsafe_get a (i + 3);
      s4 := Array1.unsafe_get a (i + 4);
      s5 := Array1.unsafe_get a (i + 5);
      s6 := Array1.unsafe_get a (i + 6);
      s7 := Array1.unsafe_get a (i + 7);
      s8 := Array1.unsafe_get a (i + 8);
      s9 := Array1.unsafe_get a (i + 9)
    done;
    s := !s + !s0 + !s1 + !s2 + !s3 + !s4 + !s5 + !s6 + !s7 + !s8 + !s9
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int8_signed get" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int8_signed c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let x = 0 in
    for j = 0 to 9999 do
      Array1.unsafe_set a (i + 0) x;
      Array1.unsafe_set a (i + 1) x;
      Array1.unsafe_set a (i + 2) x;
      Array1.unsafe_set a (i + 3) x;
      Array1.unsafe_set a (i + 4) x;
      Array1.unsafe_set a (i + 5) x;
      Array1.unsafe_set a (i + 6) x;
      Array1.unsafe_set a (i + 7) x;
      Array1.unsafe_set a (i + 8) x;
      Array1.unsafe_set a (i + 9) x
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int8_signed set" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int8_unsigned c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let s0 = ref 0 in
    let s1 = ref 0 in
    let s2 = ref 0 in
    let s3 = ref 0 in
    let s4 = ref 0 in
    let s5 = ref 0 in
    let s6 = ref 0 in
    let s7 = ref 0 in
    let s8 = ref 0 in
    let s9 = ref 0 in
    for j = 0 to 9999 do
      s0 := Array1.unsafe_get a (i + 0);
      s1 := Array1.unsafe_get a (i + 1);
      s2 := Array1.unsafe_get a (i + 2);
      s3 := Array1.unsafe_get a (i + 3);
      s4 := Array1.unsafe_get a (i + 4);
      s5 := Array1.unsafe_get a (i + 5);
      s6 := Array1.unsafe_get a (i + 6);
      s7 := Array1.unsafe_get a (i + 7);
      s8 := Array1.unsafe_get a (i + 8);
      s9 := Array1.unsafe_get a (i + 9)
    done;
    s := !s + !s0 + !s1 + !s2 + !s3 + !s4 + !s5 + !s6 + !s7 + !s8 + !s9
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int8_unsigned get" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int8_unsigned c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let x = 0 in
    for j = 0 to 9999 do
      Array1.unsafe_set a (i + 0) x;
      Array1.unsafe_set a (i + 1) x;
      Array1.unsafe_set a (i + 2) x;
      Array1.unsafe_set a (i + 3) x;
      Array1.unsafe_set a (i + 4) x;
      Array1.unsafe_set a (i + 5) x;
      Array1.unsafe_set a (i + 6) x;
      Array1.unsafe_set a (i + 7) x;
      Array1.unsafe_set a (i + 8) x;
      Array1.unsafe_set a (i + 9) x
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int8_unsigned set" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int16_signed c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let s0 = ref 0 in
    let s1 = ref 0 in
    let s2 = ref 0 in
    let s3 = ref 0 in
    let s4 = ref 0 in
    let s5 = ref 0 in
    let s6 = ref 0 in
    let s7 = ref 0 in
    let s8 = ref 0 in
    let s9 = ref 0 in
    for j = 0 to 9999 do
      s0 := Array1.unsafe_get a (i + 0);
      s1 := Array1.unsafe_get a (i + 1);
      s2 := Array1.unsafe_get a (i + 2);
      s3 := Array1.unsafe_get a (i + 3);
      s4 := Array1.unsafe_get a (i + 4);
      s5 := Array1.unsafe_get a (i + 5);
      s6 := Array1.unsafe_get a (i + 6);
      s7 := Array1.unsafe_get a (i + 7);
      s8 := Array1.unsafe_get a (i + 8);
      s9 := Array1.unsafe_get a (i + 9)
    done;
    s := !s + !s0 + !s1 + !s2 + !s3 + !s4 + !s5 + !s6 + !s7 + !s8 + !s9
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int16_signed get" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int16_signed c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let x = 0 in
    for j = 0 to 9999 do
      Array1.unsafe_set a (i + 0) x;
      Array1.unsafe_set a (i + 1) x;
      Array1.unsafe_set a (i + 2) x;
      Array1.unsafe_set a (i + 3) x;
      Array1.unsafe_set a (i + 4) x;
      Array1.unsafe_set a (i + 5) x;
      Array1.unsafe_set a (i + 6) x;
      Array1.unsafe_set a (i + 7) x;
      Array1.unsafe_set a (i + 8) x;
      Array1.unsafe_set a (i + 9) x
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int16_signed set" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int16_unsigned c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let s0 = ref 0 in
    let s1 = ref 0 in
    let s2 = ref 0 in
    let s3 = ref 0 in
    let s4 = ref 0 in
    let s5 = ref 0 in
    let s6 = ref 0 in
    let s7 = ref 0 in
    let s8 = ref 0 in
    let s9 = ref 0 in
    for j = 0 to 9999 do
      s0 := Array1.unsafe_get a (i + 0);
      s1 := Array1.unsafe_get a (i + 1);
      s2 := Array1.unsafe_get a (i + 2);
      s3 := Array1.unsafe_get a (i + 3);
      s4 := Array1.unsafe_get a (i + 4);
      s5 := Array1.unsafe_get a (i + 5);
      s6 := Array1.unsafe_get a (i + 6);
      s7 := Array1.unsafe_get a (i + 7);
      s8 := Array1.unsafe_get a (i + 8);
      s9 := Array1.unsafe_get a (i + 9)
    done;
    s := !s + !s0 + !s1 + !s2 + !s3 + !s4 + !s5 + !s6 + !s7 + !s8 + !s9
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int16_unsigned get" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int16_unsigned c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let x = 0 in
    for j = 0 to 9999 do
      Array1.unsafe_set a (i + 0) x;
      Array1.unsafe_set a (i + 1) x;
      Array1.unsafe_set a (i + 2) x;
      Array1.unsafe_set a (i + 3) x;
      Array1.unsafe_set a (i + 4) x;
      Array1.unsafe_set a (i + 5) x;
      Array1.unsafe_set a (i + 6) x;
      Array1.unsafe_set a (i + 7) x;
      Array1.unsafe_set a (i + 8) x;
      Array1.unsafe_set a (i + 9) x
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int16_unsigned set" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int32 c_layout (10 * 10000) in
  let s = ref 0l in
  for i = 0 to 9999 do
    let i = i * 10 in
    let s0 = ref 0l in
    let s1 = ref 0l in
    let s2 = ref 0l in
    let s3 = ref 0l in
    let s4 = ref 0l in
    let s5 = ref 0l in
    let s6 = ref 0l in
    let s7 = ref 0l in
    let s8 = ref 0l in
    let s9 = ref 0l in
    for j = 0 to 9999 do
      s0 := Array1.unsafe_get a (i + 0);
      s1 := Array1.unsafe_get a (i + 1);
      s2 := Array1.unsafe_get a (i + 2);
      s3 := Array1.unsafe_get a (i + 3);
      s4 := Array1.unsafe_get a (i + 4);
      s5 := Array1.unsafe_get a (i + 5);
      s6 := Array1.unsafe_get a (i + 6);
      s7 := Array1.unsafe_get a (i + 7);
      s8 := Array1.unsafe_get a (i + 8);
      s9 := Array1.unsafe_get a (i + 9)
    done;
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %ld\n%!" "int32 get" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int32 c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let x = 0l in
    for j = 0 to 9999 do
      Array1.unsafe_set a (i + 0) x;
      Array1.unsafe_set a (i + 1) x;
      Array1.unsafe_set a (i + 2) x;
      Array1.unsafe_set a (i + 3) x;
      Array1.unsafe_set a (i + 4) x;
      Array1.unsafe_set a (i + 5) x;
      Array1.unsafe_set a (i + 6) x;
      Array1.unsafe_set a (i + 7) x;
      Array1.unsafe_set a (i + 8) x;
      Array1.unsafe_set a (i + 9) x
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int32 set" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int64 c_layout (10 * 10000) in
  let s = ref 0L in
  for i = 0 to 9999 do
    let i = i * 10 in
    let s0 = ref 0L in
    let s1 = ref 0L in
    let s2 = ref 0L in
    let s3 = ref 0L in
    let s4 = ref 0L in
    let s5 = ref 0L in
    let s6 = ref 0L in
    let s7 = ref 0L in
    let s8 = ref 0L in
    let s9 = ref 0L in
    for j = 0 to 9999 do
      s0 := Array1.unsafe_get a (i + 0);
      s1 := Array1.unsafe_get a (i + 1);
      s2 := Array1.unsafe_get a (i + 2);
      s3 := Array1.unsafe_get a (i + 3);
      s4 := Array1.unsafe_get a (i + 4);
      s5 := Array1.unsafe_get a (i + 5);
      s6 := Array1.unsafe_get a (i + 6);
      s7 := Array1.unsafe_get a (i + 7);
      s8 := Array1.unsafe_get a (i + 8);
      s9 := Array1.unsafe_get a (i + 9)
    done;
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %Ld\n%!" "int64 get" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int64 c_layout (10 * 10000) in
  let s = ref 0L in
  for i = 0 to 9999 do
    let i = i * 10 in
    let x = 0L in
    for j = 0 to 9999 do
      Array1.unsafe_set a (i + 0) x;
      Array1.unsafe_set a (i + 1) x;
      Array1.unsafe_set a (i + 2) x;
      Array1.unsafe_set a (i + 3) x;
      Array1.unsafe_set a (i + 4) x;
      Array1.unsafe_set a (i + 5) x;
      Array1.unsafe_set a (i + 6) x;
      Array1.unsafe_set a (i + 7) x;
      Array1.unsafe_set a (i + 8) x;
      Array1.unsafe_set a (i + 9) x
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %Ld\n%!" "int64 set" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let s0 = ref 0 in
    let s1 = ref 0 in
    let s2 = ref 0 in
    let s3 = ref 0 in
    let s4 = ref 0 in
    let s5 = ref 0 in
    let s6 = ref 0 in
    let s7 = ref 0 in
    let s8 = ref 0 in
    let s9 = ref 0 in
    for j = 0 to 9999 do
      s0 := Array1.unsafe_get a (i + 0);
      s1 := Array1.unsafe_get a (i + 1);
      s2 := Array1.unsafe_get a (i + 2);
      s3 := Array1.unsafe_get a (i + 3);
      s4 := Array1.unsafe_get a (i + 4);
      s5 := Array1.unsafe_get a (i + 5);
      s6 := Array1.unsafe_get a (i + 6);
      s7 := Array1.unsafe_get a (i + 7);
      s8 := Array1.unsafe_get a (i + 8);
      s9 := Array1.unsafe_get a (i + 9)
    done;
    s := !s + !s0 + !s1 + !s2 + !s3 + !s4 + !s5 + !s6 + !s7 + !s8 + !s9
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int get" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create int c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let x = 0 in
    for j = 0 to 9999 do
      Array1.unsafe_set a (i + 0) x;
      Array1.unsafe_set a (i + 1) x;
      Array1.unsafe_set a (i + 2) x;
      Array1.unsafe_set a (i + 3) x;
      Array1.unsafe_set a (i + 4) x;
      Array1.unsafe_set a (i + 5) x;
      Array1.unsafe_set a (i + 6) x;
      Array1.unsafe_set a (i + 7) x;
      Array1.unsafe_set a (i + 8) x;
      Array1.unsafe_set a (i + 9) x
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "int set" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create nativeint c_layout (10 * 10000) in
  let s = ref 0n in
  for i = 0 to 9999 do
    let i = i * 10 in
    let s0 = ref 0n in
    let s1 = ref 0n in
    let s2 = ref 0n in
    let s3 = ref 0n in
    let s4 = ref 0n in
    let s5 = ref 0n in
    let s6 = ref 0n in
    let s7 = ref 0n in
    let s8 = ref 0n in
    let s9 = ref 0n in
    for j = 0 to 9999 do
      s0 := Array1.unsafe_get a (i + 0);
      s1 := Array1.unsafe_get a (i + 1);
      s2 := Array1.unsafe_get a (i + 2);
      s3 := Array1.unsafe_get a (i + 3);
      s4 := Array1.unsafe_get a (i + 4);
      s5 := Array1.unsafe_get a (i + 5);
      s6 := Array1.unsafe_get a (i + 6);
      s7 := Array1.unsafe_get a (i + 7);
      s8 := Array1.unsafe_get a (i + 8);
      s9 := Array1.unsafe_get a (i + 9)
    done;
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %nd\n%!" "nativeint get" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array1.create nativeint c_layout (10 * 10000) in
  let s = ref 0 in
  for i = 0 to 9999 do
    let i = i * 10 in
    let x = 0n in
    for j = 0 to 9999 do
      Array1.unsafe_set a (i + 0) x;
      Array1.unsafe_set a (i + 1) x;
      Array1.unsafe_set a (i + 2) x;
      Array1.unsafe_set a (i + 3) x;
      Array1.unsafe_set a (i + 4) x;
      Array1.unsafe_set a (i + 5) x;
      Array1.unsafe_set a (i + 6) x;
      Array1.unsafe_set a (i + 7) x;
      Array1.unsafe_set a (i + 8) x;
      Array1.unsafe_set a (i + 9) x
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %d\n%!" "nativeint set" (t1 -. t0) !s;

  let t0 = Unix.gettimeofday () in
  let a = Array2.create float64 c_layout 10000 10 in
  let s = ref 0. in
  for i = 0 to 9999 do
    let s0 = ref 0. in
    let s1 = ref 0. in
    let s2 = ref 0. in
    let s3 = ref 0. in
    let s4 = ref 0. in
    let s5 = ref 0. in
    let s6 = ref 0. in
    let s7 = ref 0. in
    let s8 = ref 0. in
    let s9 = ref 0. in
    for j = 0 to 9999 do
      s0 := Array2.unsafe_get a i 0;
      s1 := Array2.unsafe_get a i 1;
      s2 := Array2.unsafe_get a i 2;
      s3 := Array2.unsafe_get a i 3;
      s4 := Array2.unsafe_get a i 4;
      s5 := Array2.unsafe_get a i 5;
      s6 := Array2.unsafe_get a i 6;
      s7 := Array2.unsafe_get a i 7;
      s8 := Array2.unsafe_get a i 8;
      s9 := Array2.unsafe_get a i 9
    done;
    s := !s +. !s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7 +. !s8 +. !s9
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %f\n%!" "Array2 float64 get" (t1 -. t0) (!s +. 0.);

  let t0 = Unix.gettimeofday () in
  let a = Array2.create float64 c_layout 10000 10 in
  let s = ref 0. in
  for i = 0 to 9999 do
    let x = 0. in
    for j = 0 to 9999 do
      Array2.unsafe_set a i 0 x;
      Array2.unsafe_set a i 1 x;
      Array2.unsafe_set a i 2 x;
      Array2.unsafe_set a i 3 x;
      Array2.unsafe_set a i 4 x;
      Array2.unsafe_set a i 5 x;
      Array2.unsafe_set a i 6 x;
      Array2.unsafe_set a i 7 x;
      Array2.unsafe_set a i 8 x;
      Array2.unsafe_set a i 9 x
    done;
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %f\n%!" "Array2 float64 set" (t1 -. t0) (!s +. 0.);

  let t0 = Unix.gettimeofday () in
  let a = Array3.create float64 c_layout 10 1000 10 in
  let s = ref 0. in
  for i = 0 to 9 do
    for k = 0 to 999 do
      let s0 = ref 0. in
      let s1 = ref 0. in
      let s2 = ref 0. in
      let s3 = ref 0. in
      let s4 = ref 0. in
      let s5 = ref 0. in
      let s6 = ref 0. in
      let s7 = ref 0. in
      let s8 = ref 0. in
      let s9 = ref 0. in
      for j = 0 to 9999 do
        s0 := Array3.unsafe_get a i k 0;
        s1 := Array3.unsafe_get a i k 1;
        s2 := Array3.unsafe_get a i k 2;
        s3 := Array3.unsafe_get a i k 3;
        s4 := Array3.unsafe_get a i k 4;
        s5 := Array3.unsafe_get a i k 5;
        s6 := Array3.unsafe_get a i k 6;
        s7 := Array3.unsafe_get a i k 7;
        s8 := Array3.unsafe_get a i k 8;
        s9 := Array3.unsafe_get a i k 9
      done;
      s := !s +. !s0 +. !s1 +. !s2 +. !s3 +. !s4 +. !s5 +. !s6 +. !s7 +. !s8 +. !s9
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %f\n%!" "Array3 float64 get" (t1 -. t0) (!s +. 0.);

  let t0 = Unix.gettimeofday () in
  let a = Array3.create float64 c_layout 10 1000 10 in
  for i = 0 to 9 do
    for k = 0 to 999 do
      let x = 0. in
      for j = 0 to 9999 do
        Array3.unsafe_set a i k 0 x;
        Array3.unsafe_set a i k 1 x;
        Array3.unsafe_set a i k 2 x;
        Array3.unsafe_set a i k 3 x;
        Array3.unsafe_set a i k 4 x;
        Array3.unsafe_set a i k 5 x;
        Array3.unsafe_set a i k 6 x;
        Array3.unsafe_set a i k 7 x;
        Array3.unsafe_set a i k 8 x;
        Array3.unsafe_set a i k 9 x
      done
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-24s %f %f\n%!" "Array3 float64 set" (t1 -. t0) (!s +. 0.);


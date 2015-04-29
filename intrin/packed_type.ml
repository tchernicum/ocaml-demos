(* Demonstrates packed types possible using inline assembly *)

(* Field  Type    Size
   A      int     64
   B      int     32
   C      int     16
   D      int     8
   E      int     64
   F      float   32
   G      double  64
*)
module Foo : sig
  type t

  val create : unit -> t
  val get_a : t -> int
  val set_a : t -> int -> unit
  val get_b : t -> int
  val set_b : t -> int -> unit
  val get_c : t -> int
  val set_c : t -> int -> unit
  val get_d : t -> int
  val set_d : t -> int -> unit
  val get_e : t -> int
  val set_e : t -> int -> unit
  val get_f : t -> float
  val set_f : t -> float -> unit
  val get_g : t -> float
  val set_g : t -> float -> unit
end = struct
  type t = bytes

  let len = 8 + 4 + 2 + 1 + 8 + 4 + 8

  let create () = Bytes.create len

  external get_a : t -> int         = "%asm" ""
       "mov	(%0), %1	# get_a" "r" "=r"
  external set_a : t -> int -> unit = "%asm" ""
       "mov	%1, (%0)	# set_a" "r" "r" ""
  external get_b : t -> int         = "%asm" ""
       "movsxd	8(%0), %1	# get_b" "r" "=r"
  external set_b : t -> int -> unit = "%asm" ""
       "mov%L1	%k1, 8(%0)	# set_b" "r" "g" ""
  external get_c : t -> int         = "%asm" ""
       "movsx%W1	12(%0), %1	# get_c" "r" "=r"
  external set_c : t -> int -> unit = "%asm" ""
       "mov%W1	%w1, 12(%0)	# set_c" "r" "g" ""
  external get_d : t -> int         = "%asm" ""
       "movsx%B1	14(%0), %1	# get_d" "r" "=r"
  external set_d : t -> int -> unit = "%asm" ""
       "mov%B1	%b1, 14(%0)	# set_d" "r" "g" ""
  external get_e : t -> int         = "%asm" ""
       "mov	15(%0), %1	# get_e" "r" "=r"
  external set_e : t -> int -> unit = "%asm" ""
       "mov	%1, 15(%0)	# set_e" "r" "r" ""
  external get_f : t -> float         = "%asm" ""
       "cvtss2sd	23(%0), %x1	# get_f" "r" "=x"
  external set_f : t -> float -> float -> int -> unit = "%asm" ""
       "cvtsd2ss	%x1, %x2	# set_f
	movq	%x2, %3
	mov	%3, 23(%0)" "r" "x" "x" "r" ""
  let set_f t x = set_f t x 0. 1
  external get_g : t -> int -> float         = "%asm" ""
       "mov	27(%0), %1	# get_g
	movq	%1, %x2" "r" "r" "=x"
  let get_g t = get_g t 1
  external set_g : t -> float -> int -> unit = "%asm" ""
       "movq	%x1, %2	# set_g
	mov	%2, 27(%0)" "r" "x" "r" ""
  let set_g t x = set_g t x 1
end

module Foo_c = struct
  type t = bytes

  let len = 8 + 4 + 2 + 1 + 8 + 4 + 8

  let create () = Bytes.create len

  external get_a : t -> int = "get_a" "noalloc"
  external get_b : t -> int = "get_b" "noalloc"
  external get_c : t -> int = "get_c" "noalloc"
  external get_d : t -> int = "get_d" "noalloc"
  external get_e : t -> int = "get_e" "noalloc"
  external get_f : t -> float = "get_f"
  external get_g : t -> float = "get_g"
  external set_a : t -> int -> unit = "set_a" "noalloc"
  external set_b : t -> int -> unit = "set_b" "noalloc"
  external set_c : t -> int -> unit = "set_c" "noalloc"
  external set_d : t -> int -> unit = "set_d" "noalloc"
  external set_e : t -> int -> unit = "set_e" "noalloc"
  external set_f : t -> float -> unit = "set_f" "noalloc"
  external set_g : t -> float -> unit = "set_g" "noalloc"
end

let () =
  let foo = Foo.create () in
  Foo.set_a foo 0xf0f0f0f0f;
  Foo.set_b foo 0xf0f0f;
  Foo.set_c foo 0xf0f;
  Foo.set_d foo 0xf;
  Foo.set_e foo 0xf0f0f0f0f;
  Foo.set_f foo 23.;
  Foo.set_g foo 1231.;
  assert (Foo.get_a foo = 0xf0f0f0f0f);
  assert (Foo.get_b foo = 0xf0f0f);
  assert (Foo.get_c foo = 0xf0f);
  assert (Foo.get_d foo = 0xf);
  assert (Foo.get_e foo = 0xf0f0f0f0f);
  assert (Foo.get_f foo = 23.);
  assert (Foo.get_g foo = 1231.);

  let foo = Foo_c.create () in
  Foo_c.set_a foo 0xf0f0f0f0f;
  Foo_c.set_b foo 0xf0f0f;
  Foo_c.set_c foo 0xf0f;
  Foo_c.set_d foo 0xf;
  Foo_c.set_e foo 0xf0f0f0f0f;
  Foo_c.set_f foo 23.;
  Foo_c.set_g foo 1231.;
  assert (Foo_c.get_a foo = 0xf0f0f0f0f);
  assert (Foo_c.get_b foo = 0xf0f0f);
  assert (Foo_c.get_c foo = 0xf0f);
  assert (Foo_c.get_d foo = 0xf);
  assert (Foo_c.get_e foo = 0xf0f0f0f0f);
  assert (Foo_c.get_f foo = 23.);
  assert (Foo_c.get_g foo = 1231.);

  let t0 = Unix.gettimeofday () in
  let foo = Foo_c.create () in
  let n = 1_000_000 in
  let x = 23 in
  let y = 23. in
  let sa = ref 0 in
  let sb = ref 0 in
  let sc = ref 0 in
  let sd = ref 0 in
  let se = ref 0 in
  let sf = ref 0. in
  let sg = ref 0. in
  for i = 1 to n do
    Foo_c.set_a foo x;
    Foo_c.set_b foo x;
    Foo_c.set_c foo x;
    Foo_c.set_d foo x;
    Foo_c.set_e foo x;
    Foo_c.set_f foo y;
    Foo_c.set_g foo y;
    sa := Foo_c.get_a foo;
    sb := Foo_c.get_b foo;
    sc := Foo_c.get_c foo;
    sd := Foo_c.get_d foo;
    se := Foo_c.get_e foo;
    sf := Foo_c.get_f foo;
    sg := Foo_c.get_g foo;
  done;
  let t0 = t0 +. 0. *. (float_of_int (!sa + !sb + !sc + !sd + !se) +. !sf +. !sg) in
  let t1 = Unix.gettimeofday () in
  let foo = Foo.create () in
  let x = 23 in
  let y = 23. in
  let sa = ref 0 in
  let sb = ref 0 in
  let sc = ref 0 in
  let sd = ref 0 in
  let se = ref 0 in
  let sf = ref 0. in
  let sg = ref 0. in
  for i = 1 to n do
    Foo.set_a foo x;
    Foo.set_b foo x;
    Foo.set_c foo x;
    Foo.set_d foo x;
    Foo.set_e foo x;
    Foo.set_f foo y;
    Foo.set_g foo y;
    sa := Foo.get_a foo;
    sb := Foo.get_b foo;
    sc := Foo.get_c foo;
    sd := Foo.get_d foo;
    se := Foo.get_e foo;
    sf := Foo.get_f foo;
    sg := Foo.get_g foo;
  done;
  let t1 = t1 +. 0. *. (float_of_int (!sa + !sb + !sc + !sd + !se) +. !sf +. !sg) in
  let t2 = Unix.gettimeofday () in
  let t2 = t2 -. t1 in
  let t1 = t1 -. t0 in
  let ns_mult = 1_000_000_000. /. (float_of_int n) in
  Printf.printf "%f ns vs %f ns, speedup %f\n"
    (t1 *. ns_mult) (t2 *. ns_mult) (t1 /. t2)

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

  let len = 8 + 4 + 2 + 1 + 4 + 8

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

external func1 : int ref -> int = "%asm" "mov	(%0), %1" "r" "=r"
let a = ref 5
let () =
  let b = func1 a in
  assert (!a = b)

external func2 : int -> int ref -> unit = "%asm" "mov	%0, (%1)" "r" "r" "" "memory"
let a = ref 5
let () =
  let b = 1 in
  func2 b a;
  assert (!a = b)

external func3 : int -> int = "%asm" "mov	%0, %1" "r" "=r"
let () =
  let a = 5 in
  let b = func3 a in
  assert (a = b)

external func4 : int -> unit = "%asm" "addq	$0x100, %0" "+r" "" "cc"
let () =
  let b = !a in
  func4 b;
  assert (b = 129)

external func5 : int -> int = "%asm" "mov	$0x100, %1
	add	%0, %1" "r" "=&r" "cc"
let () =
  let a = 5 in
  let b = func5 a in
  assert (b = 133)

external func6 : int -> int = "%asm" "mov	$0x100, %1
	add	%0, %1" "r" "=r" "cc"
let () =
  let a = 5 in
  let b = func6 a in
  assert (b <> 133)

external func7 : int -> int -> int = "%asm" "add	%1, %0
	dec	%0
	mov	%0, %2" "r" "r" "=r" "cc"
let () =
  let a = 5 in
  let b = 6 in
  let c = func7 a b in
  assert (c = 11)

external func8 : int -> int -> int = "%asm" "add	%1, %2
	dec	%2" "2" "r" "=r" "cc"
let () =
  let a = 5 in
  let b = 6 in
  let c = func8 a b in
  assert (c = 11)

external func9 : int -> int = "%asm" "mov	%0, %1" "m" "=r"
let a = ref 5
let () =
  let b = func9 !a in
  assert (b = 5)

external func10 : int -> unit = "%asm" "addq	$2, %%rax" "+a" "" "cc"
let () =
  let b = !a in
  func10 b;
  assert (b = 6)

external func11 : int -> int -> unit = "%asm" "add	%0, %1
	dec	%1" "r" "+d" "" "cc"
let () =
  let b = !a in
  let c = !a in
  func11 b c;
  assert (c = 10)

external func12 : int -> int -> int -> int
  = "%asm" "dec	%1
	mul	%1
	inc     %1
	inc	%2
	dec	%3" "2" "r" "=a" "=d" "cc"
let () =
  a := 2 lsl 31;
  let b = !a in
  let c = !a in
  let d = !a in
  let e = func12 b c d in
  assert (d = b);
  assert (e = 1)

external func13 : int -> int -> int -> int
  = "%asm" "dec	%1
	mul	%1
	inc	%1
	inc	%2
	dec	%3" "2" "D" "=a" "=d" "cc"
let () =
  a := 2 lsl 31;
  let b = !a in
  let c = !a in
  let d = !a in
  let e = func13 b c d in
  assert (d = b);
  assert (e = 1)

external func14 : int -> int -> unit = "%asm" "add	%0, %1
	dec	%1" "rm" "+r" "" "cc"
let () =
  a := 5;
  let b = !a in
  func14 !a b;
  assert (b = 10)

external func15 : int -> int -> unit = "%asm" "add	%0, %1
	dec	%1" "g" "+r" "" "cc"
let () =
  let b = !a in
  func15 5 b;
  assert (b = 10);

external func16 : int -> int -> unit = "%asm" "add	%0, %1
	dec	%1" "i" "+r" "" "cc"
let () =
  let b = !a in
  func16 5 b;
  assert (b = 10)

external func17 : int -> int -> unit = "%asm" "add	%0, %1
	dec	%1" "m#hello" "+r" "" "cc"
let () =
  let b = !a in
  func17 !a b;
  assert (b = 10)

external func18 : int -> int -> unit = "%asm" "add	%0, %1
	decq	%1" "r,m,r" "+m,r,r" ",," "cc"
let () =
  let b = !a in
  let c = !a + 1 in
  func18 b c;
  assert (c = 11);
  func18 !a b;
  assert (b = 10);
  func18 b !a;
  assert (!a = 15)

external func20a : int -> int -> unit = "%asm" "add	%0, %1
	decq	%1" "d?,r" "+r?,a" "," "cc"
external func20b : int -> int -> unit = "%asm" "add	%0, %1
	decq	%1" "d,r?" "+r,a?" "," "cc"
let () =
  a := 5;
  let b = !a in
  let c = !a + 1 in
  func20a b c;
  func20b b c;
  assert (c = 16)

external func21a : int -> int -> unit = "%asm" "add	%0, %1
	decq	%1" "d??,r?" "+r??,a?" "," "cc"
external func21b : int -> int -> unit = "%asm" "add	%0, %1
	decq	%1" "d?,r??" "+r?,a??" "," "cc"
let () =
  let b = !a in
  let c = !a + 1 in
  func21a b c;
  func21b b c;
  assert (c = 16)

external func22a : int -> int -> unit = "%asm" "add	%0, %1
	decq	%1" "d!,r??" "+r!,a??" "," "cc"
external func22b : int -> int -> unit = "%asm" "add	%0, %1
	decq	%1" "d??,r!" "+r??,a!" "," "cc"
let () =
  let b = !a in
  let c = !a + 1 in
  func22a b c;
  func22b b c;
  assert (c = 16)

external func23 : int -> int -> unit = "%asm" "sar	%1
	decq	%0
	mul	%0
	incq	%0
	incq	%1" "g" "+a" "" "%rdx" "cc"
let () =
  let b = !a in
  let c = !a + 1 in
  func23 b c;
  assert (c = 30)

external func24 : int -> int -> unit = "%asm" "sar	%1
	decq	%0
	mul	%0
	incq	%0
	incq	%1" "g" "+a" "" "%rdi" "%rsi" "%rdx" "%rcx" "%r8" "%r9" "memory" "cc"
let () =
  let b = !a in
  let c = !a + 1 in
  func24 b c;
  assert (c = 30)

external func25 : int -> int -> int -> int -> int = "%asm" "mov	%0, %2
	mov	%1, %3
	shr	$10, %2
	shl	$10, %3
	add	%0, %2
	lea	(%1, %3, 1), %4
	xor	%2, %4
	incq	%3
	incq	%4" "g" "g" "=&r" "=&r" "=r" "cc"
let () =
  let b = !a in
  let c = !a + 1 in
  let d = !a + 2 in
  let e = !a + 3 in
  let f = func25 b c d e in
  assert (d = 5);
  assert (e = 6656);
  assert (f = 6659)

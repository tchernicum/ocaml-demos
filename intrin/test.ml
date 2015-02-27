external func1 : int ref -> int = "%asm" "mov	(%0), %1" "r" "=r"
let () =
  let a = ref 5 in
  let b = func1 a in
  Printf.printf "%d %d\n" !a b;

external func2 : int -> int ref -> unit = "%asm" "mov	%0, (%1)" "r" "r" "" "memory"
let () =
  let a = ref 5 in
  let b = !a + 1 in
  func2 b a;
  let c = !a + 2 in
  Printf.printf "%d %d %d\n" !a b c

external func3 : int -> int = "%asm" "mov	%0, %1" "r" "=r"
let () =
  let a = 5 in
  let b = func3 a in
  Printf.printf "%d %d\n" a b

external func4 : int -> unit = "%asm" "addq	$0xf0, %0" "+r" "" "cc"
let () =
  let a = !(ref 5) in
  func4 a;
  Printf.printf "%d\n" a

external func5 : int -> int = "%asm" "mov	$0x10, %1
	add	%0, %1" "r" "=&r" "cc"
let () =
  let a = 5 in
  let b = func5 a in
  Printf.printf "%d %d\n" a b

external func6 : int -> int = "%asm" "mov	$0x10, %1
	add	%0, %1" "r" "=r" "cc"
let () =
  let a = 5 in
  let b = func6 a in
  Printf.printf "%d %d\n" a b

external func7 : int -> int -> int = "%asm" "add	%1, %0
	mov	%0, %2" "r" "r" "=r" "cc"
let () =
  let a = 5 in
  let b = 6 in
  let c = func7 a b in
  Printf.printf "%d %d %d\n" a b c

external func8 : int -> int -> int = "%asm" "add	%1, %2" "2" "r" "=r" "cc"
let () =
  let a = 5 in
  let b = 6 in
  let c = func8 a b in
  Printf.printf "%d %d %d\n" a b c

external func9 : int -> int = "%asm" "mov	%0, %1" "m" "=r"
let a = ref 5
let () =
  let b = func9 !a in
  Printf.printf "%d %d\n" !a b

external func10 : int -> unit = "%asm" "inc	%%rax" "+a" "" "cc"
let () =
  let a = ref 5 in
  func10 !a;
  Printf.printf "%d\n" !a

external func11 : int -> int -> unit = "%asm" "add	%0, %1" "r" "+d" "" "cc"
let () =
  let a = ref 5 in
  let b = ref 6 in
  func11 !a !b;
  Printf.printf "%d %d\n" !a !b

external func12 : int -> int -> int -> int
  = "%asm" "mul	%1" "%2" "r" "=a" "=d" "cc"
let () =
  let a = 5 in
  let b = 6 in
  let c = ref 7 in
  let d = func12 a b !c in
  Printf.printf "%d %d %d %d\n" a b !c d

external func13 : int -> int -> int -> int
  = "%asm" "mul	%1" "%2" "D" "=a" "=d" "cc"
let () =
  let a = 5 in
  let b = 6 in
  let c = ref 7 in
  let d = func12 a b !c in
  Printf.printf "%d %d %d %d\n" a b !c d


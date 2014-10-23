let test_letrec x y =
  let x = x +. 0. in
  let y = y +. 0. in
  let z = let rec z = x +. y in z +. y in
  z +. z

let () =
  assert (test_letrec 2. 3. = 16.)

type t = A | B | C of int | D of int

let test_switch m x y =
  let x = x +. 0. in
  let y = y +. 0. in
  let z =
    match m with
    | A -> x +. y
    | B -> x -. y
    | C _ -> y -. x
    | D _ -> x *. y
  in
  z +. z

let () =
  assert (test_switch A 1. 2. = 6.);
  assert (test_switch B 1. 2. = -2.);
  assert (test_switch (C 1) 1. 2. = 2.);
  assert (test_switch (D 2) 2. 4. = 16.)

let test_stringswitch m x y =
  let x = x +. 0. in
  let y = y +. 0. in
  let z =
    match m with
    | "A" -> x +. y
    | _   -> x *. y
  in
  z +. z

let () =
  assert (test_stringswitch "A" 5. 3. = 16.);
  assert (test_stringswitch "B" 5. 3. = 30.)

type t2 = A | B | C | D

let test_catch m x y =
  let x = x +. 0. in
  let y = y +. 0. in
  let z =
    match m with
    | A | C -> x +. y
    | _ -> x -. y
  in
  z +. z

let () =
  assert (test_catch A 5. 3. = 16.);
  assert (test_catch B 5. 3. = 4.);
  assert (test_catch C 5. 3. = 16.);
  assert (test_catch D 5. 3. = 4.)

let test_trywith m x y =
  let x = x +. 0. in
  let y = y +. 0. in
  let z =
    try
      if m = 0 then raise Not_found;
      x +. y
    with _ -> x -. y
  in
  z +. z

let () =
  assert (test_trywith 0 5. 3. = 4.);
  assert (test_trywith 1 5. 3. = 16.)

let test_ifthenelse m x y =
  let x = x +. 0. in
  let y = y +. 0. in
  let z = if m > 0 then x +. y else x -. y in
  z +. z

let () =
  assert (test_ifthenelse 1 5. 3. = 16.);
  assert (test_ifthenelse 0 5. 3. = 4.)

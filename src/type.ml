type var = int
type t = Int | Bool | Fun of t * t | Var of var

let counter = ref 0

let fresh_var () =
  incr counter;
  Var !counter

let reset_freshness = counter := 0

let compare t1 t2 =
  match (t1, t2) with
  | Int, Int | Bool, Bool -> 0
  | Fun (p1, r1), Fun (p2, r2) -> (
      match compare p1 p2 with 0 -> compare r1 r2 | v -> v)
  | Var v1, Var v2 -> compare v1 v2
  | Int, Bool | Int, Fun (_, _) | Int, Var _ -> -1
  | Bool, Int -> compare t2 t1
  | Bool, Fun (_, _) | Bool, Var _ -> -1
  | Fun (_, _), Int | Fun (_, _), Bool -> compare t2 t1
  | Fun (_, _), Var _ -> -1
  | Var _, Int | Var _, Bool | Var _, Fun (_, _) -> compare t2 t1

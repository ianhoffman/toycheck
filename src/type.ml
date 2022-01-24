type var = int
type ty = Int | Bool | Fun of ty * ty | Var of var

let counter = ref 0

let fresh_var () =
  incr counter;
  Var !counter

let reset_freshness = counter := 0

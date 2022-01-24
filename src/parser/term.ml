open Toycheck_utils.Print_util

type term =
  | Int of int
  | Bool of bool
  | Fun of string * term
  | Var of string
  | App of term * term
  | If of term * term * term
  | Let of string * term * term

let rec pp t indent =
  match t with
  | Int v -> print_with_indent (string_of_int v) indent
  | Bool v -> print_with_indent (string_of_bool v) indent
  | Fun (binding, body) ->
      print_with_indent "Fn(" indent;
      print_with_indent binding (indent + 4);
      pp body (indent + 4);
      print_with_indent ")" indent
  | Var v1 -> print_with_indent ("Var(" ^ v1 ^ ")") indent
  | App (t1, t2) ->
      print_with_indent "App(" indent;
      pp t1 (indent + 4);
      pp t2 (indent + 4);
      print_with_indent ")" indent
  | If (c, l, r) ->
      print_with_indent "If(" indent;
      pp c (indent + 4);
      pp l (indent + 4);
      pp r (indent + 4);
      print_with_indent ")" indent
  | Let (binding, value, body) ->
      print_with_indent "Let(" indent;
      print_with_indent binding (indent + 4);
      pp value (indent + 4);
      pp body (indent + 4);
      print_with_indent ")" indent

let rec equal t1 t2 =
  match (t1, t2) with
  | Int v1, Int v2 -> v1 = v2
  | Bool v1, Bool v2 -> v1 = v2
  | Fun (binding1, body1), Fun (binding2, body2) ->
      binding1 = binding2 && equal body1 body2
  | Var v1, Var v2 -> v1 = v2
  | App (fn1, val1), App (fn2, val2) -> equal fn1 fn2 && equal val1 val2
  | If (cond1, l1, r1), If (cond2, l2, r2) ->
      equal cond1 cond2 && equal l1 l2 && equal r1 r2
  | Let (b1, v1, body1), Let (b2, v2, body2) ->
      b1 = b2 && equal v1 v2 && equal body1 body2
  | _ -> false

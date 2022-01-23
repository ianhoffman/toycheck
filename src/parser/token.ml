type token =
  | If
  | Then
  | Else
  | Fn
  | Darrow
  | Let
  | In
  | End
  | Val
  | Equal
  | True
  | False
  | LParen
  | RParen
  | Add
  | Sub
  | Int of int
  | Var of string

let show_token tok =
  match tok with
  | If -> "If"
  | Then -> "Then"
  | Else -> "Else"
  | Fn -> "Fn"
  | Darrow -> "Darrow"
  | Let -> "Let"
  | In -> "In"
  | End -> "End"
  | Val -> "Val"
  | Equal -> "Equal"
  | True -> "True"
  | False -> "False"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Add -> "Add"
  | Sub -> "Sub"
  | Int(v) -> "Int(" ^ string_of_int v ^ ")"
  | Var(v) -> "Var(" ^ v ^ ")"

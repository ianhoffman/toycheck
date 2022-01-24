type t =
  | Binding of Type.ty * string
  | Int of Type.ty * int
  | Bool of Type.ty * bool
  | Fun of Type.ty * t * t
  | Var of Type.ty * string
  | App of Type.ty * t * t
  | If of Type.ty * t * t * t
  | Let of Type.ty * t * t * t

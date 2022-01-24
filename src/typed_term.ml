type typed_term =
  | Binding of Type.ty * string
  | Int of Type.ty * int
  | Bool of Type.ty * bool
  | Fun of Type.ty * typed_term * typed_term
  | Var of Type.ty * string
  | App of Type.ty * typed_term * typed_term
  | If of Type.ty * typed_term * typed_term * typed_term
  | Let of Type.ty * typed_term * typed_term * typed_term

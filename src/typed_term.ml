type binding = { t : Type.t; name : string }

type t =
  | Int of { t : Type.t; value : int }
  | Bool of { t : Type.t; value : bool }
  | Fun of { t : Type.t; param : binding; body : t }
  | Var of { t : Type.t; name : string }
  | App of { t : Type.t; fn : t; arg : t }
  | If of { t : Type.t; cond : t; left : t; right : t }
  | Let of { t : Type.t; binding : binding; value : t; body : t }

let extract_ty term =
  match term with
  | Int { t; _ }
  | Bool { t; _ }
  | Fun { t; _ }
  | Var { t; _ }
  | App { t; _ }
  | If { t; _ }
  | Let { t; _ } ->
      t

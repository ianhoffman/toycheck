module Constraint = struct
  type t = { a : Type.t; b : Type.t }

  let compare t1 t2 =
    match compare t1.a t2.a with 0 -> compare t1.b t2.b | v -> v
end

module ConstraintSet = Set.Make (Constraint)

let rec collect t =
  match t with
  | Typed_term.Int { t; _ } -> ConstraintSet.singleton { a = t; b = Int }
  | Typed_term.Bool { t; _ } -> ConstraintSet.singleton { a = t; b = Bool }
  | Typed_term.Fun { t; param; body } ->
      collect body
      |> ConstraintSet.add
           { a = t; b = Fun (param.t, Typed_term.extract_ty body) }
  | Typed_term.Var _ -> ConstraintSet.empty
  | Typed_term.App { t; fn; arg } ->
      collect arg
      |> ConstraintSet.union (collect fn)
      |> ConstraintSet.add
           {
             (* A function application makes that the function applied must be of
                of type (arg.t -> t). Put differently, t must be the return
                type of the function and arg.t must be the type of its parameter.
             *)
             a = Typed_term.extract_ty fn;
             b = Type.Fun (Typed_term.extract_ty arg, t);
           }
  | Typed_term.If { t; cond; left; right } ->
      collect right
      |> ConstraintSet.union (collect left)
      |> ConstraintSet.union (collect cond)
      |> ConstraintSet.add { a = Typed_term.extract_ty cond; b = Bool }
      |> ConstraintSet.add { a = Typed_term.extract_ty left; b = t }
      |> ConstraintSet.add { a = Typed_term.extract_ty right; b = t }
  | Typed_term.Let { t; binding; value; body } ->
      collect body
      |> ConstraintSet.union (collect value)
      |> ConstraintSet.add { a = t; b = Typed_term.extract_ty body }
      |> ConstraintSet.add { a = binding.t; b = Typed_term.extract_ty value }

open Toycheck_parser
module TEnv = Map.Make (String)

let rec annotate term tenv =
  match term with
  | Term.Int v -> Typed_term.Int (Type.fresh_var (), v)
  | Term.Bool v -> Typed_term.Bool (Type.fresh_var (), v)
  | Term.Fun (param, body) ->
      let param_ty = Type.fresh_var () in
      let param_binding = Typed_term.Binding (param_ty, param) in
      let extended_env = TEnv.add param param_ty tenv in
      Typed_term.Fun
        (Type.fresh_var (), param_binding, annotate body extended_env)
  | Term.Var name -> (
      match TEnv.find_opt name tenv with
      | None -> raise (Failure ("unbound identifier: " ^ name))
      | Some ty -> Typed_term.Var (ty, name))
  | Term.App (fn, arg) ->
      Typed_term.App (Type.fresh_var (), annotate fn tenv, annotate arg tenv)
  | Term.If (cond, left, right) ->
      Typed_term.If
        ( Type.fresh_var (),
          annotate cond tenv,
          annotate left tenv,
          annotate right tenv )
  | Term.Let (binding, value, body) ->
      let binding_ty = Type.fresh_var () in
      let extended_env = TEnv.add binding binding_ty tenv in
      Typed_term.Let
        ( Type.fresh_var (),
          Typed_term.Binding (binding_ty, binding),
          annotate value tenv,
          annotate body extended_env )

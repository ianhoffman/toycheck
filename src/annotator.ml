open Toycheck_parser
module TEnv = Map.Make (String)

let rec annotate term tenv =
  match term with
  | Term.Int v -> Typed_term.Int { t = Type.fresh_var (); value = v }
  | Term.Bool v -> Typed_term.Bool { t = Type.fresh_var (); value = v }
  | Term.Fun (param, body) ->
      let param_ty = Type.fresh_var () in
      Typed_term.Fun
        {
          t = Type.fresh_var ();
          param = { t = param_ty; name = param };
          body = annotate body (TEnv.add param param_ty tenv);
        }
  | Term.Var name -> (
      match TEnv.find_opt name tenv with
      | None -> raise (Failure ("unbound identifier: " ^ name))
      | Some ty -> Typed_term.Var { t = ty; name })
  | Term.App (fn, arg) ->
      Typed_term.App
        {
          t = Type.fresh_var ();
          fn = annotate fn tenv;
          arg = annotate arg tenv;
        }
  | Term.If (cond, left, right) ->
      Typed_term.If
        {
          t = Type.fresh_var ();
          cond = annotate cond tenv;
          left = annotate left tenv;
          right = annotate right tenv;
        }
  | Term.Let (binding, value, body) ->
      let binding_ty = Type.fresh_var () in
      let extended_env = TEnv.add binding binding_ty tenv in
      Typed_term.Let
        {
          t = Type.fresh_var ();
          binding = { t = binding_ty; name = binding };
          value = annotate value tenv;
          body = annotate body extended_env;
        }

open OUnit2
open Toycheck
open Toycheck.Constraint

let test_contrains_int _ =
  let t1 = Type.fresh_var () in
  let typed_term = Typed_term.Int { t = t1; value = 1 } in
  assert_equal (collect typed_term)
    (ConstraintSet.singleton { a = t1; b = Type.Int })

let test_constrains_bool _ =
  let t1 = Type.fresh_var () in
  let typed_term = Typed_term.Bool { t = t1; value = true } in
  assert_equal (collect typed_term)
    (ConstraintSet.singleton { a = t1; b = Type.Bool })

let test_constrains_fun _ =
  let t1 = Type.fresh_var () in
  let t2 = Type.fresh_var () in
  let t3 = Type.fresh_var () in

  let param = Typed_term.bind t2 "param" in
  let body = Typed_term.Var { t = t3; name = "param" } in
  let typed_term = Typed_term.Fun { t = t1; param; body } in
  assert_equal (collect typed_term)
    (ConstraintSet.singleton { a = t1; b = Type.Fun (t2, t3) })

let test_constrains_var _ =
  let t1 = Type.fresh_var () in
  let typed_term = Typed_term.Var { t = t1; name = "name" } in
  assert_equal (collect typed_term) ConstraintSet.empty

let test_constrains_app _ =
  let t1 = Type.fresh_var () in
  let t2 = Type.fresh_var () in
  let t3 = Type.fresh_var () in
  let fn = Typed_term.Var { t = t2; name = "fn" } in
  let arg = Typed_term.Var { t = t3; name = "arg" } in
  let typed_term = Typed_term.App { t = t1; fn; arg } in
  assert_equal (collect typed_term)
    (ConstraintSet.singleton { a = t2; b = Type.Fun (t3, t1) })

let test_constrains_if _ =
  let t1 = Type.fresh_var () in
  let t2 = Type.fresh_var () in
  let t3 = Type.fresh_var () in
  let t4 = Type.fresh_var () in
  let test_condition = Typed_term.Var { t = t2; name = "test" } in
  let true_branch = Typed_term.Var { t = t3; name = "true" } in
  let false_branch = Typed_term.Var { t = t4; name = "false" } in
  let typed_term =
    Typed_term.If
      {
        t = t1;
        cond = test_condition;
        left = true_branch;
        right = false_branch;
      }
  in
  assert_equal (collect typed_term)
    (ConstraintSet.empty
    |> ConstraintSet.add { a = t2; b = Type.Bool }
    |> ConstraintSet.add { a = t3; b = t1 }
    |> ConstraintSet.add { a = t4; b = t1 })

let test_constrains_let _ =
  let t1 = Type.fresh_var () in
  let t2 = Type.fresh_var () in
  let t3 = Type.fresh_var () in
  let t4 = Type.fresh_var () in
  let binding = Typed_term.bind t2 "name" in
  let value = Typed_term.Var { t = t3; name = "value" } in
  let body = Typed_term.Var { t = t4; name = "body" } in
  let typed_term = Typed_term.Let { t = t1; binding; value; body } in
  assert_equal (collect typed_term)
    (ConstraintSet.empty
    |> ConstraintSet.add { a = t1; b = t4 }
    |> ConstraintSet.add { a = t2; b = t3 })

let suite =
  "test_constraint"
  >::: [
         "test_constrains_int" >:: test_contrains_int;
         "test_constrains_bool" >:: test_constrains_bool;
         "test_constrains_fun" >:: test_constrains_fun;
         "test_constrains_var" >:: test_constrains_var;
         "test_constrains_app" >:: test_constrains_app;
         "test_constrains_if" >:: test_constrains_if;
         "test_constrains_let" >:: test_constrains_let;
       ]

let () = run_test_tt_main suite

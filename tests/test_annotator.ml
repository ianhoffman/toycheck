open OUnit2
open Toycheck
open Toycheck_parser

let tenv =
  TEnv.empty
  |> TEnv.add "+" (Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)))
  |> TEnv.add "-" (Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)))

let test_annotate_identity _ =
  match annotate (Term.Fun ("a", Term.Var "a")) tenv with
  | Fun (Var t1, Binding (Var t2, "a"), Var (Var t3, "a")) ->
      assert_bool "t1 = t2" (t1 != t2);
      assert_bool "t1 = t3" (t1 != t3);
      assert_bool "t2 != t3" (t2 = t3)
  | _ -> assert_failure "Unexpected ret"

let test_annotate_const _ =
  match annotate (Term.Fun ("a", Term.Fun ("b", Term.Var "a"))) tenv with
  | Fun
      ( Var t1,
        Binding (Var t2, "a"),
        Fun (Var t3, Binding (Var t4, "b"), Var (Var t5, "a")) ) ->
      assert_bool "t1 = t2 || t1 = t3 || t1 = t4 || t1 = t5"
        (t1 != t2 && t1 != t3 && t1 != t4 && t1 != t5);
      assert_bool "t2 = t3 || t2 = t4" (t2 != t3 && t2 != t4);
      assert_bool "t3 = t4 || t3 = t5" (t3 != t4 && t3 != t5);
      assert_bool "t4 = t5" (t4 != t5);
      assert_bool "t2 = t5" (t2 = t5)
  | _ -> assert_failure "Unexpected ret"

let test_annotate_compose _ =
  match
    annotate
      (Term.Fun
         ( "f",
           Term.Fun
             ( "g",
               Term.Fun
                 ( "x",
                   Term.App (Term.Var "f", Term.App (Term.Var "g", Term.Var "x"))
                 ) ) ))
      tenv
  with
  | Fun
      ( Var t1,
        Binding (Var t2, "f"),
        Fun
          ( Var t3,
            Binding (Var t4, "g"),
            Fun
              ( Var t5,
                Binding (Var t6, "x"),
                App
                  ( Var t7,
                    Var (Var t8, "f"),
                    App (Var t9, Var (Var t10, "g"), Var (Var t11, "x")) ) ) )
      ) ->
      assert_bool "t1"
        (t1 != t2 && t1 != t3 && t1 != t4 && t1 != t5 && t1 != t6 && t1 != t7
       && t1 != t8 && t1 != t9 && t1 != t10 && t1 != t11);
      assert_bool "t2"
        (t2 != t3 && t2 != t4 && t2 != t5 && t2 != t6 && t2 != t7 && t2 = t8
       && t2 != t9 && t2 != t10 && t2 != t11);
      assert_bool "t3"
        (t3 != t4 && t3 != t5 && t3 != t6 && t3 != t7 && t3 != t8 && t3 != t9
       && t3 != t10 && t3 != t11);
      assert_bool "t4"
        (t4 != t5 && t4 != t6 && t4 != t7 && t4 != t8 && t4 != t9 && t4 = t10
       && t4 != t11);
      assert_bool "t5"
        (t5 != t6 && t5 != t7 && t5 != t8 && t1 != t5 && t5 != t10 && t5 != t11);
      assert_bool "t6"
        (t6 != t7 && t6 != t8 && t6 != t9 && t6 != t10 && t6 = t11);
      assert_bool "t7" (t7 != t8 && t7 != t9 && t7 != t10 && t7 != t11);
      assert_bool "t8" (t8 != t9 && t8 != t10 && t8 != t11);
      assert_bool "t9" (t9 != t10 && t9 != t11);
      assert_bool "t10" (t10 != t11)
  | _ -> assert_failure "Unexpected ret"

let suite =
  "test_annotator"
  >::: [
         "test_annotate_identity" >:: test_annotate_identity;
         "test_annotate_const" >:: test_annotate_const;
         "test_annotate_compose" >:: test_annotate_compose;
       ]

let () = run_test_tt_main suite

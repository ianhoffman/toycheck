open OUnit2
open Toycheck
open Toycheck_parser

let not_contains term terms = not (List.mem terms term)

let tenv =
  TEnv.empty
  |> TEnv.add "+" (Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)))
  |> TEnv.add "-" (Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)))

let test_annotate_identity _ =
  match annotate (Term.Fun ("a", Term.Var "a")) tenv with
  | Fun
      {
        t = Var t1;
        param = { t = Var t2; name = "a" };
        body = Var { t = Var t3; name = "a" };
      } ->
      assert_bool "t1" (t1 != t2 && t1 != t3);
      assert_bool "t2" (t2 = t3)
  | _ -> assert_failure "Unexpected ret"

let test_annotate_const _ =
  match annotate (Term.Fun ("a", Term.Fun ("b", Term.Var "a"))) tenv with
  | Fun
      {
        t = Var t1;
        param = { t = Var t2; name = "a" };
        body =
          Fun
            {
              t = Var t3;
              param = { t = Var t4; name = "b" };
              body = Var { t = Var t5; name = "a" };
            };
      } ->
      assert_bool "t1" (t1 != t2 && t1 != t3 && t1 != t4 && t1 != t5);
      assert_bool "t2" (t2 != t3 && t2 != t4 && t2 = t5);
      assert_bool "t3" (t3 != t4 && t3 != t5);
      assert_bool "t4" (t4 != t5)
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
      {
        t = Var t1;
        param = { t = Var t2; name = "f" };
        body =
          Fun
            {
              t = Var t3;
              param = { t = Var t4; name = "g" };
              body =
                Fun
                  {
                    t = Var t5;
                    param = { t = Var t6; name = "x" };
                    body =
                      App
                        {
                          t = Var t7;
                          fn = Var { t = Var t8; name = "f" };
                          arg =
                            App
                              {
                                t = Var t9;
                                fn = Var { t = Var t10; name = "g" };
                                arg = Var { t = Var t11; name = "x" };
                              };
                        };
                  };
            };
      } ->
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

let test_annotate_pred _ =
  match
    annotate
      (Term.Fun
         ( "pred",
           Term.If
             (Term.App (Term.Var "pred", Term.Int 1), Term.Int 2, Term.Int 3) ))
      tenv
  with
  | Fun
      {
        t = Var t1;
        param = { t = Var t2; name = "pred" };
        body =
          If
            {
              t = Var t3;
              cond =
                App
                  {
                    t = Var t4;
                    fn = Var { t = Var t5; name = "pred" };
                    arg = Int { t = Var t6; value = 1 };
                  };
              left = Int { t = Var t7; value = 2 };
              right = Int { t = Var t8; value = 3 };
            };
      } ->
      assert_bool "t1" (not_contains [ t2; t3; t4; t5; t6; t7; t8 ] t1);
      assert_bool "t2" (not_contains [ t3; t4; t6; t7; t8 ] t2 && t2 = t5);
      assert_bool "t3" (not_contains [ t4; t5; t6; t7; t8 ] t3);
      assert_bool "t4" (not_contains [ t5; t6; t7; t8 ] t4);
      assert_bool "t5" (not_contains [ t6; t7; t8 ] t5);
      assert_bool "t6" (not_contains [ t7; t8 ] t6);
      assert_bool "t6" (not_contains [ t8 ] t7)
  | _ -> assert_failure "Unexpected ret"

let test_annotate_inc _ =
  match
    annotate
      (Term.Let
         ( "inc",
           Term.Fun
             ("a", Term.App (Term.App (Term.Var "+", Term.Var "a"), Term.Int 1)),
           Term.Let
             ( "dec",
               Term.Fun
                 ( "a",
                   Term.App (Term.App (Term.Var "-", Term.Var "a"), Term.Int 1)
                 ),
               Term.App (Term.Var "dec", Term.App (Term.Var "inc", Term.Int 42))
             ) ))
      tenv
  with
  | Let
      {
        t = Var t1;
        binding = { t = Var t2; name = "inc" };
        value =
          Fun
            {
              t = Var t3;
              param = { t = Var t4; name = "a" };
              body =
                App
                  {
                    t = Var t5;
                    fn =
                      App
                        {
                          t = Var t6;
                          fn = Var { t = Fun (Int, Fun (Int, Int)); name = "+" };
                          arg = Var { t = Var t7; name = "a" };
                        };
                    arg = Int { t = Var t8; value = 1 };
                  };
            };
        body =
          Let
            {
              t = Var t9;
              binding = { t = Var t10; name = "dec" };
              value =
                Fun
                  {
                    t = Var t11;
                    param = { t = Var t12; name = "a" };
                    body =
                      App
                        {
                          t = Var t13;
                          fn =
                            App
                              {
                                t = Var t14;
                                fn =
                                  Var
                                    {
                                      t = Fun (Int, Fun (Int, Int));
                                      name = "-";
                                    };
                                arg = Var { t = Var t15; name = "a" };
                              };
                          arg = Int { t = Var t16; value = 1 };
                        };
                  };
              body =
                App
                  {
                    t = Var t17;
                    fn = Var { t = Var t18; name = "dec" };
                    arg =
                      App
                        {
                          t = Var t19;
                          fn = Var { t = Var t20; name = "inc" };
                          arg = Int { t = Var t21; value = 42 };
                        };
                  };
            };
      } ->
      assert_bool "t1"
        (not_contains
           [
             t2;
             t3;
             t4;
             t5;
             t6;
             t7;
             t8;
             t9;
             t10;
             t11;
             t12;
             t13;
             t14;
             t15;
             t16;
             t17;
             t18;
             t19;
             t20;
             t21;
           ]
           t1);
      assert_bool "t2"
        (not_contains
           [
             t3;
             t4;
             t5;
             t6;
             t7;
             t8;
             t9;
             t10;
             t11;
             t12;
             t13;
             t14;
             t15;
             t16;
             t17;
             t18;
             t19;
             t21;
           ]
           t2
        && t2 = t20);
      assert_bool "t3"
        (not_contains
           [
             t4;
             t5;
             t6;
             t7;
             t8;
             t9;
             t10;
             t11;
             t12;
             t13;
             t14;
             t15;
             t16;
             t17;
             t18;
             t19;
             t20;
             t21;
           ]
           t3);
      assert_bool "t4"
        (not_contains
           [
             t5;
             t6;
             t8;
             t9;
             t10;
             t11;
             t12;
             t13;
             t14;
             t15;
             t16;
             t17;
             t18;
             t19;
             t20;
             t21;
           ]
           t4
        && t4 = t7);
      assert_bool "t5"
        (not_contains
           [
             t6;
             t7;
             t8;
             t9;
             t10;
             t11;
             t12;
             t13;
             t14;
             t15;
             t16;
             t17;
             t18;
             t19;
             t20;
             t21;
           ]
           t5);
      assert_bool "t6"
        (not_contains
           [
             t7;
             t8;
             t9;
             t10;
             t11;
             t12;
             t13;
             t14;
             t15;
             t16;
             t17;
             t18;
             t19;
             t20;
             t21;
           ]
           t6);
      assert_bool "t7"
        (not_contains
           [
             t8; t9; t10; t11; t12; t13; t14; t15; t16; t17; t18; t19; t20; t21;
           ]
           t7);
      assert_bool "t8"
        (not_contains
           [ t9; t10; t11; t12; t13; t14; t15; t16; t17; t18; t19; t20; t21 ]
           t8);
      assert_bool "t9"
        (not_contains
           [ t10; t11; t12; t13; t14; t15; t16; t17; t18; t19; t20; t21 ]
           t9);
      assert_bool "t10"
        (not_contains [ t11; t12; t13; t14; t15; t16; t17; t19; t20; t21 ] t10
        && t10 = t18);
      assert_bool "t11"
        (not_contains [ t12; t13; t14; t15; t16; t17; t18; t19; t20; t21 ] t11);
      assert_bool "t12"
        (not_contains [ t13; t14; t16; t17; t18; t19; t20; t21 ] t12
        && t12 = t15);
      assert_bool "t13"
        (not_contains [ t14; t15; t16; t17; t18; t19; t20; t21 ] t13);
      assert_bool "t14" (not_contains [ t15; t16; t17; t18; t19; t20; t21 ] t14);
      assert_bool "t15" (not_contains [ t16; t17; t18; t19; t20; t21 ] t15);
      assert_bool "t16" (not_contains [ t17; t18; t19; t20; t21 ] t16);
      assert_bool "t17" (not_contains [ t18; t19; t20; t21 ] t17);
      assert_bool "t18" (not_contains [ t19; t20; t21 ] t18);
      assert_bool "t19" (not_contains [ t20; t21 ] t19);
      assert_bool "t20" (not_contains [ t21 ] t20)
  | _ -> assert_failure "Unexpected ret"

let suite =
  "test_annotator"
  >::: [
         "test_annotate_identity" >:: test_annotate_identity;
         "test_annotate_const" >:: test_annotate_const;
         "test_annotate_compose" >:: test_annotate_compose;
         "test_annotate_pred" >:: test_annotate_pred;
         "test_annotate_inc" >:: test_annotate_inc;
       ]

let () = run_test_tt_main suite

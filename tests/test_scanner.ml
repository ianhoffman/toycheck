open OUnit2
open Toycheck_parser.Token

let scan = Toycheck.scan

let test_scans_identity _ =
  scan {|
    fn a => a
  |}
  |> assert_equal [
    Fn;
    Var("a");
    Darrow;
    Var("a")
  ]

let test_scans_const _ =
  scan {|
    fn a => fn b => a
  |}
  |> assert_equal [
    Fn; 
    Var("a");
    Darrow;
    Fn;
    Var("b");
    Darrow;
    Var("a")
  ]

let test_scans_compose _ =
  scan {|
    fn f => fn g => fn x => f (g x)
  |}
  |> assert_equal [
    Fn;
    Var("f");
    Darrow;
    Fn;
    Var("g");
    Darrow;
    Fn;
    Var("x");
    Darrow;
    Var("f");
    LParen;
    Var("g");
    Var("x");
    RParen;
  ]

let test_scans_pred _ =
  scan {|
    fn pred => if pred 1 then 2 else 3
  |}
  |> assert_equal [
    Fn;
    Var("pred");
    Darrow;
    If;
    Var("pred");
    Int(1);
    Then;
    Int(2);
    Else;
    Int(3);
  ]

let test_scans_inc _ =
  scan {|
    let
      val inc = fn a => a + 1
    in 
      let
        val dec = fn a => a - 1
      in
        dec (inc 42)
      end
    end
  |}
  |> assert_equal [
    Let;
    Val;
    Var("inc");
    Equal;
    Fn;
    Var("a");
    Darrow;
    Var("a");
    Add;
    Int(1);
    In;
    Let;
    Val;
    Var("dec");
    Equal;
    Fn;
    Var("a");
    Darrow;
    Var("a");
    Sub;
    Int(1);
    In;
    Var("dec");
    LParen;
    Var("inc");
    Int(42);
    RParen;
    End;
    End;
  ]

let suite =
  "test_scanner">:::
  [ "test_scans_identity">:: test_scans_identity
  ; "test_scans_cost">:: test_scans_const
  ; "test_scans_compose">:: test_scans_compose
  ; "test_scans_pred">:: test_scans_pred
  ; "test_scans_inc">:: test_scans_inc
  ]

let () = run_test_tt_main suite ;;
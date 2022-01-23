open OUnit2
open Toycheck_parser

let parse = Toycheck.parse_tokens
let eq = assert_equal ~cmp:Term.equal

let test_parse_identity _ =
  parse [
    Token.Fn; 
    Token.Var("a");
    Token.Darrow;
    Token.Var("a")
  ]
  |> eq (Term.Fun("a", Term.Var("a")))

let test_parse_const _ =
  parse [
    Token.Fn; 
    Token.Var("a");
    Token.Darrow;
    Token.Fn;
    Token.Var("b");
    Token.Darrow;
    Token.Var("a")
  ]
  |> eq (Term.Fun ("a", Term.Fun("b", Term.Var("a"))))

let test_parse_compose _ =
  parse [
    Token.Fn;
    Token.Var("f");
    Token.Darrow;
    Token.Fn;
    Token.Var("g");
    Token.Darrow;
    Token.Fn;
    Token.Var("x");
    Token.Darrow;
    Token.Var("f");
    Token.LParen;
    Token.Var("g");
    Token.Var("x");
    Token.RParen;
  ]
  |> eq (
    Term.Fun(
      "f",
      Term.Fun(
        "g",
        Term.Fun(
          "x",
          Term.App(
            Term.Var("f"),
            Term.App(
              Term.Var("g"),
              Term.Var("x")
            )
          )
        )
      )
    )
  )
  
let test_parse_pred _ =
  parse [
    Token.Fn;
    Token.Var("pred");
    Token.Darrow;
    Token.If;
    Token.Var("pred");
    Token.Int(1);
    Token.Then;
    Token.Int(2);
    Token.Else;
    Token.Int(3);
  ]
  |> eq (
    Term.Fun(
      "pred",
      Term.If(
        Term.App(
          Term.Var("pred"),
          Term.Int(1)
        ),
        Term.Int(2),
        Term.Int(3)
      )
    ) 
  )

let test_parse_inc _ =
  parse [
    Token.Let;
    Token.Val;
    Token.Var("inc");
    Token.Equal;
    Token.Fn;
    Token.Var("a");
    Token.Darrow;
    Token.Var("a");
    Token.Add;
    Token.Int(1);
    Token.In;
    Token.Let;
    Token.Val;
    Token.Var("dec");
    Token.Equal;
    Token.Fn;
    Token.Var("a");
    Token.Darrow;
    Token.Var("a");
    Token.Sub;
    Token.Int(1);
    Token.In;
    Token.Var("dec");
    Token.LParen;
    Token.Var("inc");
    Token.Int(42);
    Token.RParen;
    Token.End;
    Token.End;
  ]
  |> eq (
    Term.Let(
      "inc",
      Term.Fun(
        "a",
        Term.App(
          Term.App(Term.Var("+"), Term.Var("a")),
          Term.Int(1)
        )
      ),
      Term.Let(
        "dec",
        Term.Fun(
          "a",
          Term.App(
            Term.App(Term.Var("-"), Term.Var("a")),
            Term.Int(1)
          )
        ),
        Term.App(
          Term.Var("dec"),
          Term.App( 
            Term.Var("inc"),
            Term.Int(42)
          )
        )
      )
    )
  )

let suite =
  "test_parser">:::
  [ "test_parse_identity">:: test_parse_identity
  ; "test_parse_cost">:: test_parse_const
  ; "test_parse_compose">:: test_parse_compose
  ; "test_parse_pred">:: test_parse_pred
  ; "test_parse_inc">:: test_parse_inc
  ]

let () = run_test_tt_main suite ;;
exception UnexpectedEndOfStream of string
exception ParseError of Token.token * string

let next_is_at_exp = function
  | Token.Int _ :: _ -> true
  | Token.False :: _ -> true
  | Token.True :: _ -> true
  | Token.Var _ :: _ -> true
  | Token.Let :: _ -> true
  | Token.LParen :: _ -> true
  | _ -> false

let rec parse_let = function
  | Token.Val :: tokens -> (
      match tokens with
      | Token.Var binding_name :: tokens -> (
          match tokens with
          | Token.Equal :: tokens -> (
              let binding_val, rest = parse_exp tokens in
              match rest with
              | Token.In :: tokens -> (
                  let body, rest = parse_exp tokens in
                  match rest with
                  | Token.End :: tokens ->
                      (Term.Let (binding_name, binding_val, body), tokens)
                  | token :: _ -> raise (ParseError (token, "END"))
                  | _ -> raise (UnexpectedEndOfStream "END"))
              | token :: _ -> raise (ParseError (token, "IN"))
              | _ -> raise (UnexpectedEndOfStream "IN"))
          | token :: _ -> raise (ParseError (token, "EQUAL"))
          | _ -> raise (UnexpectedEndOfStream "EQUAL"))
      | token :: _ -> raise (ParseError (token, "VAR"))
      | _ -> raise (UnexpectedEndOfStream "VAR"))
  | token :: _ -> raise (ParseError (token, "VAL"))
  | _ -> raise (UnexpectedEndOfStream "VAL")

and parse_at_exp = function
  | Token.Int n :: tokens -> (Term.Int n, tokens)
  | Token.True :: tokens -> (Term.Bool true, tokens)
  | Token.False :: tokens -> (Term.Bool false, tokens)
  | Token.Var id :: tokens -> (Term.Var id, tokens)
  | Token.Let :: tokens -> parse_let tokens
  | Token.LParen :: tokens -> (
      let exp, rest = parse_exp tokens in
      match rest with
      | Token.RParen :: tokens -> (exp, tokens)
      | token :: _ -> raise (ParseError (token, "RPAREN"))
      | _ -> raise (UnexpectedEndOfStream "RPAREN"))
  | token :: _ ->
      raise (ParseError (token, "INT, TRUE, FALSE, VAR, LET, LPAREN"))
  | _ -> raise (UnexpectedEndOfStream "INT, TRUE, FALSE, VAR, LET, LPAREN")

and parse_app_exp tokens =
  let rec recur tokens absyn =
    if next_is_at_exp tokens then
      let inner, rest = parse_at_exp tokens in
      recur rest (Term.App (absyn, inner))
    else (absyn, tokens)
  in
  let at_exp, rest = parse_at_exp tokens in
  recur rest at_exp

and parse_inf_exp tokens =
  let rec recur tokens absyn =
    match tokens with
    | Token.Add :: tokens ->
        let inner, rest = parse_inf_exp tokens in
        recur rest (Term.App (Term.App (Term.Var "+", absyn), inner))
    | Token.Sub :: tokens ->
        let inner, rest = parse_inf_exp tokens in
        recur rest (Term.App (Term.App (Term.Var "-", absyn), inner))
    | _ -> (absyn, tokens)
  in
  let atExp, rest = parse_app_exp tokens in
  recur rest atExp

and parse_if tokens =
  let test, rest = parse_exp tokens in
  match rest with
  | Token.Then :: tokens -> (
      let left, rest = parse_exp tokens in
      match rest with
      | Token.Else :: tokens ->
          let right, rest = parse_exp tokens in
          (Term.If (test, left, right), rest)
      | token :: _ -> raise (ParseError (token, "ELSE"))
      | _ -> raise (UnexpectedEndOfStream "ELSE"))
  | token :: _ -> raise (ParseError (token, "THEN"))
  | _ -> raise (UnexpectedEndOfStream "THEN")

and parse_fn = function
  | Token.Var param :: rest -> (
      match rest with
      | Token.Darrow :: tokens ->
          let body, rest = parse_exp tokens in
          (Term.Fun (param, body), rest)
      | token :: _ -> raise (ParseError (token, "DARROW"))
      | _ -> raise (UnexpectedEndOfStream "DARROW"))
  | token :: _ -> raise (ParseError (token, "VAR"))
  | _ -> raise (UnexpectedEndOfStream "VAR")

and parse_exp = function
  | tokens when next_is_at_exp tokens -> parse_inf_exp tokens
  | Token.If :: tokens -> parse_if tokens
  | Token.Fn :: tokens -> parse_fn tokens
  | token :: _ ->
      raise (ParseError (token, "INT, TRUE, FALSE, VAR, LET, LPAREN, IF, FN"))
  | _ ->
      raise (UnexpectedEndOfStream "INT, TRUE, FALSE, VAR, LET, LPAREN, IF, FN")

let parse_tokens tokens =
  try
    match parse_exp tokens with
    | absyn, [] -> absyn
    | _, token :: _ ->
        raise (ParseError (token, "INT, TRUE, FALSE, VAR, IF, FN, LET"))
  with ParseError (t, expected) ->
    let () =
      Printf.printf "Backtrace: %s\n" (Printexc.get_backtrace ());
      Printf.printf "Got %s, expected %s\n" (Token.show_token t) expected
    in
    raise (ParseError (t, expected))

let parse s = Scanner.scan s |> parse_tokens

open Toycheck_utils
open Token

let from_string s =
  match s with
  | [] -> None
  | head::[] -> Some (head, [])
  | head::tail -> Some (head, tail)

let rec skip_white_space reader stream =
  match reader stream with
  | Some (c, stream) when CharUtil.is_white_space c ->
      skip_white_space reader stream
  | _ -> stream

let scan_digit next_char stream =
  match next_char stream with
  | Some (c, s) when CharUtil.is_digit c -> Some (CharUtil.to_digit c, s)
  | _ -> None

let scan_number next_char stream =
  let rec recur stream result =
    match scan_digit next_char stream with
    | Some (n, s) -> recur s (Some ((Option.value ~default:0 result) * 10 + n))
    | None -> Option.map (fun v -> (v, stream)) result
  in
  Option.map (fun (n, s) -> Int(n), s) (recur stream None)

let scan_ident next_char stream =
  let rec recur stream result =
    match next_char stream with
    (* TODO: Allow _ if not the first char *)
    | Some (c, s) when CharUtil.is_letter c
        -> recur s (result ^ String.make 1 c)
    | _ when result == "" -> None
    | _ -> Some (result, stream)
  in Option.map
    (fun ret -> match ret with
      | ("if", s) -> (If, s)
      | ("then", s) -> (Then, s)
      | ("else", s) -> (Else, s)
      | ("fn", s) -> (Fn, s)
      | ("let", s) -> (Let, s)
      | ("in", s) -> (In, s)
      | ("end", s) -> (End, s)
      | ("val", s) -> (Val, s)
      | ("true", s) -> (True, s)
      | ("false", s) -> (False, s)
      | (ident, s) -> (Var(ident), s)
    )
    (recur stream "")

let scan_symbol next_char stream =
  match next_char stream with
  | Some ('=', stream) -> (
      match next_char stream with
      | None -> None
      | Some ('>', stream) -> Some (Darrow, stream)
      | Some (_, stream) -> Some (Equal, stream)
    )
  | Some ('(', stream) -> Some (LParen, stream)
  | Some (')', stream) -> Some (RParen, stream)
  | Some ('+', stream) -> Some (Add, stream)
  | Some ('-', stream) -> Some (Sub, stream)
  | _ -> None

let scan_single next_char stream = 
  let s = skip_white_space next_char stream in
  match scan_number next_char s with
  | Some ret -> Some ret
  | None -> match scan_ident next_char s with
    | Some ret -> Some ret
    | None -> scan_symbol next_char s

let consume reader stream =
  let rec recur stream result =
    match reader stream with
    | None -> (result, stream)
    | Some (c, s) -> recur s (result @ [c])
  in recur stream []

let scan s =
  let (tokens, stream) =
    consume (scan_single from_string) (CharUtil.string_to_list s)
  in
  match from_string (skip_white_space from_string stream) with
  | Some (c, _) -> raise (Failure ("Unexpected character: " ^ String.make 1 c))
  | None -> tokens
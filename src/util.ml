let rec string_to_char_list s =
  match String.length s with
  | 0 -> []
  | n -> let tail = String.sub s 1 (n - 1) in 
    List.cons (String.get s 0) (string_to_char_list tail)

let char_is_white_space c =
  Char.equal c ' ' || Char.equal c '\n'

let char_is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false

let char_to_digit c = (Char.code c) - 48

let char_is_letter c =
  match c with
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false
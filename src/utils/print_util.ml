let print_with_indent s indent =
  Printf.printf "%s%s\n" (String.init indent (fun _ -> ' ')) s

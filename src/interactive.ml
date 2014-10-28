let rec read_number max =
  print_string "> "; flush stdout;
  try
    let s = input_line stdin in
    let n = int_of_string s in
    if max <> 0 && n > max then
      raise Not_found
    else n
  with _ ->
    read_number max

let read_string () =
  print_string "> "; flush stdout;
  input_line stdin

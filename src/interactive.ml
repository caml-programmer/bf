open Printf

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

let stop_delay n =
  printf "wait %d second>%!" n;
  for i = 1 to n do
    Unix.sleep 1;
    print_char ' ';
    print_int i;
    flush stdout;
  done;
  print_endline " go"

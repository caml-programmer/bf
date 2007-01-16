open Printf

let with_prefix prefix list =
  List.map (fun v -> prefix ^ "/" ^ v) list

let date_filename () =
  let tm = Unix.localtime (Unix.time ()) in
  sprintf "%04d-%02d-%02d_%02d-%02d-%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    (tm.Unix.tm_mday)
    (tm.Unix.tm_hour)
    (tm.Unix.tm_min)
    (tm.Unix.tm_sec)

let append_write ~file s =
  let ch = open_out_gen [Open_creat;Open_append] 0o644 file in
  output_string ch s;
  close_out ch

let write_buffer ~file ~buffer =
  let ch = open_out file in
  output_string ch (Buffer.contents buffer);
  close_out ch

let write_string ~file ~string =
  let ch = open_out file in
  output_string ch string;
  close_out ch

let read_file ~file =
  if Sys.file_exists file then
    let ch = open_in file in
    let buffer = Buffer.create 32 in  
    try
      while true do
	Buffer.add_string buffer (input_line ch);
	Buffer.add_string buffer "\n";
      done; ""
    with End_of_file ->
      close_in ch; Buffer.contents buffer
  else ""

let pid_exists file =
  let pid =
    let b = Buffer.create 8 in
    try
      String.iter
	(function
	  | '0' .. '9' as c -> Buffer.add_char b c
	  | _ ->  raise End_of_file) 
	(read_file ~file);
      -1
    with End_of_file -> ();
      int_of_string (Buffer.contents b)
  in Sys.command (sprintf "kill -0 %d" pid) = 0



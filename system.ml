open Printf

(* global variables *)

let path_list = ["/bin";"/sbin";"/usr/bin";"/usr/sbin"];;

let process_env = ref [||];;

(* new *)

exception Error of string

let set_process_env env =
  process_env := env
    
let get_process_env () = !process_env

let split_env_var s =
  let len = String.length s in
  let pos = String.index s '=' in
  String.sub s 0 pos,
  String.sub s (succ pos) (len - pos - 1) 
    
let string_of_exn exn =
  Printexc.to_string exn

let string_of_channel ch =
  let b = Buffer.create 32 in
  let rec read () =
    try
      read (Buffer.add_string b (input_line ch))
    with End_of_file -> Buffer.contents b
  in read ()

let list_of_channel ch =
  let rec read acc =
    try
      read (acc @ [input_line ch])
    with End_of_file -> acc
  in read []

exception Env_problem of string

let get_env name =
  try Some (Sys.getenv name) with Not_found -> None

let set_env name value =
  Unix.putenv name value

let with_alt s = function
  | Some value -> value
  | None -> s

let with_exn s = function
  | Some value -> value
  | None -> raise (Env_problem s)

let path_strip_directory file =
  let len = String.length file in
  if len > 0 then
    if file.[len-1] = '/' then
      String.sub file 0 (len - 1)
    else file
  else file

exception Cannot_create_directory of string

let create_directory_r dir =
  let rc = Sys.command (sprintf "mkdir -p %s" dir) in
  if rc <> 0 then raise (Cannot_create_directory dir)

let path_directory file =
  Filename.dirname file

let transfer_file src dst =
  let a = open_in_bin src in
  let b = open_out_bin dst in
  try
    while true do
      output_char b (input_char a)
    done
  with End_of_file ->
    close_in a; close_out b

let is_directory s =
  try
    let st = Unix.stat s in
    match st.Unix.st_kind with
      | Unix.S_DIR -> true
      | _ -> false
  with Unix.Unix_error(_,_,_) -> false

let is_regular s =
  try
    let st = Unix.stat s in
    match st.Unix.st_kind with
      | Unix.S_REG -> true
      | _ -> false
  with Unix.Unix_error(_,_,_) -> false

exception Cannot_remove_directory of string

let remove_directory dir =
  let rc = Sys.command (sprintf "rm -rf %s" dir) in
  if rc <> 0 then raise (Cannot_remove_directory dir)

let copy_file file dest =
  let name = Filename.basename file in
  if is_directory dest then
    let dest_file = Filename.concat dest name in
    transfer_file file dest_file
  else
    transfer_file file dest
    
let uname () =
  let ch = Unix.open_process_in "uname" in
  let name = String.lowercase (input_line ch) in
  close_in ch; name

let list_of_directory dir =
  let dh = Unix.opendir dir in
  let acc = ref [] in
  try
    while true do
      let s = Unix.readdir dh in
      if s <> "." && s <> ".." then
	acc := !acc @ [s];      
    done; []
  with End_of_file -> !acc

let read_lines ?(ignore_error=false) ?(filter=(fun _ -> true)) command =
  let (ch,out,err) = Unix.open_process_full command (get_process_env ()) in
  let rec read acc =
    try
      let s = input_line ch in
      if filter s then
	read (acc @ [s])
      else
	read acc
    with End_of_file ->
      if not ignore_error then
	let error = string_of_channel err in
	match Unix.close_process_full (ch,out,err) with
	  | Unix.WEXITED 0 -> acc
	  | _ ->
	      raise 
		(Error 
		  (sprintf "cannot read lines from [%s] (%s)" command error))
      else acc
  in read []


(* old *)

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


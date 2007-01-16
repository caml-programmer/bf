open Logger
open System
open Printf

let read_rules () =
  printf
    "read rules from %s/.bf-rules\n" (Sys.getcwd());
  System.read_file ~file:".bf-rules"

let build_rules () =
  let s = read_rules () in
  Scheme.eval_file s;
  Scheme.eval_code (fun _ -> ()) "(build)"

let install_rules () =
  let s = read_rules () in
  Scheme.eval_file s;
  Scheme.eval_code (fun _ -> ()) "(install)"  
  
let make () =
  log_command "make"

let install_file file dir =
  with_logger
    (fun logger ->
      log_message ~logger ("installing " ^ file);
      let dir = Filename.concat (Params.get_param "destdir") dir in
      if not (Sys.file_exists dir) then
	begin
	  log_message ~logger ("creating directory " ^ dir);
	  create_directory_r dir
	end;
      let dest_file = Filename.concat dir (path_strip_directory file) in
      if Sys.file_exists dest_file then
	Sys.remove dest_file;
      copy_file file (Filename.concat dir (path_strip_directory file)))

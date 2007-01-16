open Logger
open System
open Printf

let rules_file () =  
  let file = (Sys.getcwd()) ^ "/.bf-rules" in
  printf "load %s\n" file; file
  
let build_rules () =
  Scheme.eval_file (rules_file ());
  Scheme.eval_code (fun _ -> ()) "(build)"

let install_rules () =
  Scheme.eval_file (rules_file ());
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

open Eval
open Logger
open System

let read_rules () =
  let s = System.read_file ~file:".rules" in
  eval_reset ();
  eval_phrase s;
  eval_value "build",
  eval_value "install"

let build_rules () =
  fst (read_rules ())

let install_rules () =
  snd (read_rules ())

let make () =
  log_command "make"

let install_file file dir =
  with_logger
    (fun logger ->
      log_message ~logger ("installing " ^ file);
      let dir = Filename.concat Params.destdir dir in
      if not (Sys.file_exists dir) then
	begin
	  log_message ~logger ("creating directory " ^ dir);
	  create_directory_r dir
	end;
      let dest_file = Filename.concat dir (path_strip_directory file) in
      if Sys.file_exists dest_file then
	Sys.remove dest_file;
      copy_file file (Filename.concat dir (path_strip_directory file)))

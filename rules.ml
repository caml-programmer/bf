open Logger
open Printf

exception Not_found_plugins_directory of string

let rules_file () =
  Filename.concat (Sys.getcwd()) ".bf-rules"

let load_plugins () =
  let dir = Params.get_param "plugins-dir" in
  let plugdir =
    let start = Params.get_param "start-dir" in    
    if System.is_regular (Filename.concat dir "lib.scm") then
      dir
    else
      Filename.concat start dir
  in
  if not (System.is_regular (Filename.concat plugdir "lib.scm")) then
    raise (Not_found_plugins_directory dir);
  
  List.iter Scheme.eval_file
    (System.with_prefix plugdir (System.list_of_directory plugdir))

let load_composite file =
  let composite = ref None in
  Scheme.eval_file file;
  Scheme.eval_code (fun v -> composite := Some v) "(composite)";
  match !composite with
    | None -> log_error "composite handler is not called"
    | Some v -> v
  
let build_rules () =
  load_plugins ();
  Scheme.eval_file (rules_file ());
  Scheme.eval_code (fun _ -> ()) "(build ())"

let install_rules () =
  load_plugins ();
  Scheme.eval_file (rules_file ());
  if Sys.file_exists ".bf-build" then
    Scheme.eval_code (fun _ -> ()) "(install ())"
  else
    log_error "current component is not built"  
      
let install_file file dir =
  with_logger
    (fun logger ->
      log_message ~logger ("installing " ^ file);
      let dir = Filename.concat (Params.get_param "destdir") dir in
      if not (Sys.file_exists dir) then
	begin
	  log_message ~logger ("creating directory " ^ dir);
	  System.create_directory_r dir
	end;
      let dest_file = Filename.concat dir (System.path_strip_directory file) in
      if Sys.file_exists dest_file then
	Sys.remove dest_file;
      System.copy_file file (Filename.concat dir (System.path_strip_directory file)))



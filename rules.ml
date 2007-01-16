open Logger
open System
open Printf

let rules_file () =
  Filename.concat (Sys.getcwd()) ".bf-rules"

let load_plugins () =
  print_endline "=> load plugins";
  List.iter Scheme.eval_file
    (System.list_of_directory (Params.get_param "plugins-dir"))

let load_composite file =
  let composite = ref None in
  Scheme.eval_file file;
  Scheme.eval_code (fun v -> composite := Some v) "(composition)";
  match !composite with
    | None -> log_error "composition handler is not called"
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
	  create_directory_r dir
	end;
      let dest_file = Filename.concat dir (path_strip_directory file) in
      if Sys.file_exists dest_file then
	Sys.remove dest_file;
      copy_file file (Filename.concat dir (path_strip_directory file)))



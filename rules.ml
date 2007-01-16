open Logger
open Printf

(*** Component envirionment support *)

let component_environment = Hashtbl.create 32;;

let clear_component_env () =
  Hashtbl.clear component_environment

let make_component_env () =
  Array.iter
    (fun s ->
      (try
	let (key,value) = System.split_env_var s in
	if not (Hashtbl.mem component_environment key) then
	  Hashtbl.add component_environment key value
      with Not_found -> ()))
    (Unix.environment ());
  let f key value acc =
    acc @ [key ^ "=" ^ value] in
  let env =
    Array.of_list (Hashtbl.fold f component_environment []) in
  System.set_process_env env

(*** Rules execution *)

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
  clear_component_env ();
  Scheme.eval_file file;
  Scheme.eval_code (fun v -> composite := Some v) "(composite)";
  match !composite with
    | None -> log_error "composite handler is not called"
    | Some v -> v
  
let build_rules () =
  print_string "load plugins...";
  load_plugins ();
  clear_component_env ();
  Scheme.eval_file (rules_file ());
  print_endline "ok";
  Scheme.eval_code (fun _ -> ()) "(build ())"
    
let install_rules () =
  print_string "load plugins...";
  load_plugins ();
  clear_component_env ();
  Scheme.eval_file (rules_file ());
  print_endline "ok";
  if Sys.file_exists ".bf-build" then
    Scheme.eval_code (fun _ -> ()) "(install ())"
  else
    log_error "current component is not built"  


(*** Component rules *)

let simple_configure args =
  log_command "./configure" args
 
let simple_make args =
  log_command "make" args

let simple_install args =
  log_command "make" ("install"::args)

let export args =
  List.iter
    (fun (key,value) ->
      printf "export: %s=%s\n" key value;
      Hashtbl.replace component_environment key value)
    args;
  make_component_env ()

let make args =
  let rec prepare acc = function
    | [] -> List.rev acc
    | (key,value)::tl ->
	match value with
	  | Some v -> prepare ((key^"="^v)::acc) tl
	  | None   -> prepare (key::acc) tl
  in log_command "make" (prepare [] args)

let ac_configure args =
  let rec prepare acc = function
    | [] -> List.rev acc
    | (key,value)::tl ->
	match value with
	  | Some v -> prepare (("--"^key^"="^v)::acc) tl
	  | None   -> prepare (("--"^key)::acc) tl
  in log_command "./configure" (prepare [] args)

let path_concat args =
  let rec concat acc = function
      [] -> acc
    | hd::tl -> concat (Filename.concat acc hd) tl
  in concat "" args

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

open Ocs_env
open Ocs_types
open Logger
open Printf

(*** Rules execution *)

let rules_file name =
  match name with
    | None ->
	Filename.concat (Sys.getcwd()) ".bf-rules"
    | Some s ->
	Filename.concat (Sys.getcwd()) (".bf-rules." ^ s)

let with_snapshot snapshot f =
  Ocs_env.set_glob
    Scheme.env (Ssymbol "snapshot") (if snapshot then Strue else Sfalse);
  f ();
  Ocs_env.set_glob Scheme.env (Ssymbol "snapshot") Sfalse  

let build_rules ?(snapshot=false) name =
  Plugins.load ();
  Env.prepare ();
  print_string "load rules...";
  Scheme.eval_file (rules_file name);
  print_endline "ok"; 
  with_snapshot snapshot 
    (fun () ->
      Scheme.eval_code (fun _ -> ()) "(build ())";
      Env.prepare ())

let install_rules ?(snapshot=false) ?(check_build=true) name =
  Plugins.load ();
  Env.prepare ();
  print_string "load rules...";
  Scheme.eval_file (rules_file name);
  print_endline "ok";
  if (not check_build) || Sys.file_exists
    (match name with None -> ".bf-build" | Some s -> ".bf-build." ^ s)
  then
    begin
      with_snapshot snapshot 
	(fun () ->	  
	  Scheme.eval_code (fun _ -> ()) "(install ())";
	  Env.prepare ())
    end
  else
    log_error "current component is not built"

(*** Component rules *)

let split_by_space s =
  Pcre.split ~rex:(Re.compile (Re.rep1 Re.space)) s
  
let add_make_opts v =
  (split_by_space
    (try
      Env.find_component "MAKE_OPTS"
    with Not_found ->
      (Params.get_param "make-opts"))) @ v

let simple_configure args =
  log_command "./configure" args
    
let simple_make args =  
  log_command "make" (add_make_opts args)

let simple_install args =
  log_command "make" ("install"::args)

let export args =
  List.iter
    (fun (key,value) ->
      let key = String.uppercase key in
      match value with
	  Some v ->
	    Env.update key v
	| None ->
	    Env.update key "")
    args

let add_path args =
  List.iter (fun v -> 
    (Env.update "PATH" 
      (v ^ ":" ^ (Env.get "PATH"))))
    args

let get_env name =
  try
    Some (Env.find_component name)
  with Not_found -> None

let make args =
  let rec prepare acc = function
    | [] -> List.rev acc
    | (key,value)::tl ->
	match value with
	  | Some v -> prepare ((String.uppercase key^"="^v)::acc) tl
	  | None   -> prepare (key::acc) tl
  in log_command "make" (add_make_opts (prepare [] args))

let ac_configure args =
  let have_spaces s =
    (try
      ignore(String.index s ' '); true 
    with Not_found -> false)
  in
  let rec prepare acc = function
    | [] -> List.rev acc
    | (key,value)::tl ->
	match value with
	  | Some v -> 
	      if have_spaces v then
		prepare (("--"^key^"=\""^v^"\"")::acc) tl
	      else
		prepare (("--"^key^"="^v)::acc) tl
	  | None   -> prepare (("--"^key)::acc) tl
  in log_command "./configure" (prepare [] args)

let path_concat args =
  let rec concat acc = function
      [] -> acc
    | hd::tl -> concat (Filename.concat acc hd) tl
  in concat "" args

let string_concat args =  
  let rec concat acc = function
      [] -> acc
    | hd::tl -> concat (acc ^ hd) tl
  in concat "" args

let install_file file dir =
  with_logger
    (fun logger ->
      if not (Sys.file_exists dir) then
	begin
	  log_message ~logger ("creating directory " ^ dir);
	  System.create_directory_r dir
	end;
      let name = 
	System.path_strip_directory 
	  (Filename.basename file)
      in
      let dest_file =
	Filename.concat dir name in
      if Sys.file_exists dest_file then
	Sys.remove dest_file;
      log_message ~logger ("installing " ^ file ^ " to " ^ dest_file);
      System.copy_file file dest_file)

  
  
  








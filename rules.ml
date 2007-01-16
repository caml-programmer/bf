open Logger
open Printf

(*** Component envirionment support *)

let component_environment = Hashtbl.create 32;;

let make_component_env () =
  Array.iter
    (fun s ->
      (try
	let (key,value) = System.split_env_var s in
	if not (Hashtbl.mem component_environment key) then
	  begin	    
	    Hashtbl.add component_environment key value
	  end
      with Not_found -> ()))
    (Unix.environment ());
  let f key value acc =
    acc @ [key ^ "=" ^ value] in
  let env =
    Array.of_list (Hashtbl.fold f component_environment []) in
  System.set_process_env env

let prepare_component_env () =
  Hashtbl.clear component_environment;
  make_component_env ()

(*** Rules execution *)

let rules_file () =
  Filename.concat (Sys.getcwd()) ".bf-rules"

let load_plugins () =
  print_string "load ac-configure...";
  Scheme.eval_code (fun _ -> print_endline "ok") "
(define-syntax ac-configure
  (syntax-rules ()
    ((_ (e1 e2) ...) (ml-ac-configure '((e1 e2) ...)))
    ((_ (e1 e2))     (ml-ac-configure '((e1 e2))))
    ((_ e1 e2)       (ml-ac-configure '((e1 e2))))
    ((_ (e))         (ml-ac-configure '((e ()))))
    ((_ e)           (ml-ac-configure '((e ()))))
    ((_)             (ml-ac-configure '()))))
";
  print_string "load make...";
  Scheme.eval_code (fun _ -> print_endline "ok") "
(define-syntax make
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-make '((e ()) (e1 e2) ...)))
    ((_ (e1 e2) ...)     (ml-make '((e1 e2) ...)))
    ((_ (e1 e2))         (ml-make '((e1 e2))))
    ((_ e1 e2)           (ml-make '((e1 e2))))
    ((_ (e))             (ml-make '((e ()))))
    ((_ e)               (ml-make '((e ()))))
    ((_)                 (ml-make '()))))";

  print_string "load export...";
  Scheme.eval_code (fun _ -> print_endline "ok") "
(define-syntax export
  (syntax-rules ()
    ((_ (e1 e2) ...) (ml-export '((e1 e2) ...)))
    ((_ (e1 e2))     (ml-export '((e1 e2))))
    ((_ e1 e2)       (ml-export '((e1 e2))))
    ((_ (e))         (ml-export '((e ()))))
    ((_ e)           (ml-export '((e ()))))
    ((_)             (ml-export '()))))";

  print_string "load update-make-params...";
  Scheme.eval_code (fun _ -> print_endline "ok") "
(define-syntax update-make-params
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-update-make-params '((e ()) (e1 e2) ...)))
    ((_  e  (e1 e2) ...) (ml-update-make-params '((e ()) (e1 e2) ...)))))";

  print_string "load user lib.scm...";
  let dir = Params.get_param "plugins-dir" in
  let plugdir =
    let start = Params.get_param "start-dir" in
    if System.is_regular (Filename.concat dir "lib.scm") then
      dir
    else
      Filename.concat start dir
  in
  if not (System.is_regular (Filename.concat plugdir "lib.scm")) then
    print_endline "skip"
  else    
    begin
      System.with_extension "scm"
	Scheme.eval_file
	(System.with_prefix plugdir (System.list_of_directory plugdir));
      print_endline "ok";
    end

let load_composite file =
  let composite = ref None in
  prepare_component_env ();
  Scheme.eval_file file;
  Scheme.eval_code (fun v -> composite := Some v) "(composite)";
  match !composite with
    | None -> log_error "composite handler is not called"
    | Some v -> v
  
let build_rules () =
  load_plugins ();
  prepare_component_env ();
  print_string "load rules...";
  Scheme.eval_file (rules_file ());
  print_endline "ok";
  Scheme.eval_code (fun _ -> ()) "(build ())"
    
let install_rules () =
  load_plugins ();
  prepare_component_env ();
  print_string "load rules...";
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
      let key = String.uppercase key in
      match value with
	  Some v ->
	    Hashtbl.replace component_environment key v
	| None ->
	    Hashtbl.replace component_environment key "")
    args;
  make_component_env ()

let get_env name =
  try
    Some (Hashtbl.find component_environment name)
  with Not_found -> None

let make args =
  let rec prepare acc = function
    | [] -> List.rev acc
    | (key,value)::tl ->
	match value with
	  | Some v -> prepare ((String.uppercase key^"="^v)::acc) tl
	  | None   -> prepare (key::acc) tl
  in log_command "make" (prepare [] args)

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

let read_directory dir =
  let dh = Unix.opendir dir in
  let rec read acc = 
    try
      let s = Unix.readdir dh in
      if s <> "." && s <> ".." then
	read (acc @ [s])
      else
	read acc
    with End_of_file ->
      Unix.closedir dh; acc
  in read []

let with_dir dir f =
  let cur = Sys.getcwd () in
  try
    Sys.chdir dir;
    let r = f () in
    Sys.chdir cur;
    r
  with exn ->
    Sys.chdir cur; raise exn

let read_command cmd =
  let ch = Unix.open_process_in cmd in
  let rec read acc =
    try
      read (acc @ [input_line ch])
    with End_of_file -> 
      close_in ch; acc
  in read []

let replace_param key value content =
  let key = String.uppercase key in
  let rex =
    Pcre.regexp 
      ~flags:[`MULTILINE] ("^" ^ key ^ "\\s*=.*?$")
  in Pcre.replace 
       ~rex 
       ~templ:(key ^ "=" ^ 
       (match value with 
	 | Some v -> v 
	 | None -> "")) content

let rec replace_params content = function
  | [] -> content
  | (key,value)::tl ->
      replace_params (replace_param key value content) tl

let update_make_params v =
  let name = fst (List.hd v) in
  let params = List.tl v in
  let tmpname = name ^ ".tmp" in
  let content =    
    System.read_file ~file:name in
  System.write_string
    ~file:tmpname ~string:(replace_params content params);
  Sys.rename tmpname name

let move_file src dir =
  let name = Filename.basename src in
  let dst = Filename.concat dir name in
  Sys.rename src dst

exception Cannot_create_directory of string

let make_directory dirs =
  List.iter
    (fun dir ->
      if not (Sys.file_exists dir) then
	Unix.mkdir dir 0o755
      else if System.is_directory dir then
	log_message (sprintf "warning: directory (%s) already exists!" dir)
      else
	raise (Cannot_create_directory dir))
    dirs

let move_directory src dst =
  log_command "mv" [src;dst]

let remove_directory dir =
  log_command "rm" ["-rf";dir]
  
exception Cannot_create_symlink of (string * string)

let create_symlink src dst =
  if System.is_symlink dst && Unix.readlink dst = src
  then ()
  else
    if System.is_symlink dst then
      begin
	Unix.unlink dst;
	Unix.symlink src dst
      end
    else
      if Sys.file_exists dst then
	raise (Cannot_create_symlink (src,dst))
      else
	Unix.symlink src dst

let create_link src dst =
  Unix.link src dst

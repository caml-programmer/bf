open Ocs_env
open Ocs_types
open Logger
open Printf

(*** Rules execution *)

let debug =
  try
    ignore(Sys.getenv "DEBUG"); true
  with Not_found -> false

let rules_file name =
  match name with
    | None ->
	Filename.concat (Sys.getcwd()) ".bf-rules"
    | Some s ->
	Filename.concat (Sys.getcwd()) (".bf-rules." ^ s)

let load_plugins () =
  if debug then print_string "load ac-configure...";
  Scheme.eval_code (fun _ -> if debug then print_endline "ok") "
(define-syntax ac-configure
  (syntax-rules ()
    ((_ (e1 e2) ...) (ml-ac-configure `((e1 ,e2) ...)))
    ((_ (e1 e2))     (ml-ac-configure `((e1 ,e2))))
    ((_ e1 e2)       (ml-ac-configure `((e1 ,e2))))
    ((_ (e))         (ml-ac-configure `((e ()))))
    ((_ e)           (ml-ac-configure `((e ()))))
    ((_)             (ml-ac-configure `()))))";

  if debug then print_string "load make...";
  Scheme.eval_code (fun _ -> if debug then print_endline "ok") "
(define-syntax make
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-make `((e ()) (e1 ,e2) ...)))
    ((_ (e1 e2) ...)     (ml-make `((e1 ,e2) ...)))
    ((_ (e1 e2))         (ml-make `((e1 ,e2))))
    ((_ e1 e2)           (ml-make `((e1 ,e2))))
    ((_ (e))             (ml-make `((e ()))))
    ((_ e)               (ml-make `((e ()))))
    ((_)                 (ml-make `()))))";

  if debug then print_string "load export...";
  Scheme.eval_code (fun _ -> if debug then print_endline "ok") "
(define-syntax export
  (syntax-rules ()
    ((_ (e1 e2) ...) (ml-export `((e1 ,e2) ...)))
    ((_ (e1 e2))     (ml-export `((e1 ,e2))))
    ((_ e1 e2)       (ml-export `((e1 ,e2))))
    ((_ (e))         (ml-export `((e ()))))
    ((_ e)           (ml-export `((e ()))))
    ((_)             (ml-export `()))))";

  if debug then print_string "load update-make-params...";
  Scheme.eval_code (fun _ -> if debug then print_endline "ok") "
(define-syntax update-make-params
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-update-make-params `((e ()) (e1 ,e2) ...)))
    ((_  e  (e1 e2) ...) (ml-update-make-params `((e ()) (e1 ,e2) ...)))))";

  if debug then print_string "load user lib.scm...";
  let dir = Params.get_param "plugins-dir" in
  let plugdir =
    let start = Params.get_param "start-dir" in
    if System.is_regular (Filename.concat dir "lib.scm") then
      dir
    else
      Filename.concat start dir
  in
  if not (System.is_regular (Filename.concat plugdir "lib.scm")) then
    (if debug then print_endline "skip")
  else
    begin
      System.with_extension "scm"
	Scheme.eval_file
	(System.with_prefix plugdir (System.list_of_directory plugdir));
      (if debug then print_endline "ok");
    end

let load_composite file =
  let composite = ref None in
  Env.prepare ();
  Scheme.eval_file file;
  Scheme.eval_code (fun v -> composite := Some v) "(composite)";
  match !composite with
    | None -> log_error "composite handler is not called"
    | Some v -> v

let write_composite file components =
  let ch = open_out file in
  let out = output_string ch in
  let write c =
    out "(";
    out c.Types.name;
    (match c.Types.label with
      | Types.Current -> ()
      | Types.Branch s ->
	  out " (branch \"";
	  out s;
	  out "\")";
      | Types.Tag s ->
	  out " (tag \"";
	  out s;
	  out "\")");
    (match c.Types.pkg with
      | None -> ()
      | Some s ->
	  out " (package \"";
	  out s;
	  out "\")");
    if c.Types.nopack then
	  out " (nopack)";
    (match c.Types.rules with
      | None -> ()
      | Some s ->
	  out " (rules \"";
	  out s;
	  out "\")");   
    out ")\n";
  in
  out "(define (composite)\n'(";
  List.iter write components;
  out "))\n";
  close_out ch

let components_of_composite composite =
  let composite = load_composite composite in
  let rec iter acc = function
    | Snull -> acc
    | Spair v ->
	(match v.cdr with
	  | Snull -> (Scheme.component_of_sval v.car)::acc
	  | Spair v2 ->
	      iter ((Scheme.component_of_sval v.car)::acc) (Spair v2)
	  | _ -> log_error "invalid composition")
    | _ -> log_error "invalid composition"
  in List.rev (iter [] composite)

let build_rules name =
  load_plugins ();
  Env.prepare ();
  print_string "load rules...";
  Scheme.eval_file (rules_file name);
  print_endline "ok";
  Scheme.eval_code (fun _ -> ()) "(build ())";
  Env.prepare ()

let install_rules ?(check_build=true) name =
  load_plugins ();
  Env.prepare ();
  print_string "load rules...";
  Scheme.eval_file (rules_file name);
  print_endline "ok";
  if (not check_build) || Sys.file_exists
    (match name with None -> ".bf-build" | Some s -> ".bf-build." ^ s)
  then
    begin
      Scheme.eval_code (fun _ -> ()) "(install ())";
      Env.prepare ();
    end
  else
    log_error "current component is not built"

(*** Component rules *)

let split re s =
  let rec make acc pos =
    try
      let (a,b) =
	Re.get_ofs (Re.exec ~pos re s) 0 in
      Printf.printf "a:%d\nb:%d\npos:%d\n%!" a b pos;
      make ((String.sub s pos a)::acc) b
    with Not_found -> acc
  in make [] 0
  
let split_by_space s =
  split (Re.compile (Re.rep1 Re.space)) s
  
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

let read_directory dir =
  let dh = Unix.opendir dir in
  let rec read acc = 
    try
      let s = Unix.readdir dh in
      if s <> "." && s <> ".." then
	read (s::acc)
      else
	read acc
    with End_of_file ->
      Unix.closedir dh; List.rev acc
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
  System.read_lines ~env:(Env.component ()) cmd

let replace_param key value content =
  let replace line =
    if Re.execp (Re_perl.compile_pat ("^" ^ key ^ "\\s*=.*?$")) line then
      (key ^ "=" ^
      (match value with
	| Some v -> v
	| None -> ""))
    else
      line
  in String.concat "\n"
       (List.map replace
	 (Strings.split '\n' content))

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

let make_directory_r ?(mode=0o755) s =
  let rec make rest s =
    let dir = Filename.dirname s in
    if System.is_directory dir then
      begin
	Unix.mkdir s mode;
	List.iter
	  (fun s -> Unix.mkdir s mode)
	  rest
      end
    else
      make (s::rest) dir
  in make [] (System.path_strip_directory s)

let make_directory dirs =
  List.iter
    (fun dir ->
      if not (Sys.file_exists dir) then
	make_directory_r ~mode:0o755 dir
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
      match dst with
	| "." ->
	    Unix.symlink src (Filename.basename src)
	|  _ ->
	     if Sys.file_exists dst then
	       raise (Cannot_create_symlink (src,dst))
	     else
	       Unix.symlink src dst

let create_link src dst =
  Unix.link src dst

let send_file_over_ssh src dst =
  log_command "scp" [src;dst]


(* Log viewer *)

let log_viewer () =
  exit (Sys.command
    (sprintf "tail -f %s" (Params.get_param "session-log")))
  
  
  








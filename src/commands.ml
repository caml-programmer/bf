open Git
open Rules
open Logger
open Ocs_types
open Printf
open Component
open Components

let read_command cmd =
  System.read_lines ~env:(Env.component ()) cmd

let replace_param key value content =
  let replace line =
    let value =
      match value with
	| Some v -> v
	| None -> "" in
    let result =
      if Re.execp (Re_perl.compile_pat ~opts:[`Caseless] ("^" ^ key ^ "\\s*=.*?$")) line then
	((String.uppercase key) ^ "=" ^ value)
      else
	line in
    (* printf "update-params key(%s) value(%s): %s\n%!" key value result; *)
    result
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

let make_directory dirs =
  List.iter
    (fun dir ->
      if not (Sys.file_exists dir) then
	System.make_directory_r ~mode:0o755 dir
      else if System.is_directory dir then
	log_message (sprintf "warning: directory (%s) already exists!" dir)
      else
	raise (System.Cannot_create_directory dir))
    dirs

let move_directory src dst =
  log_command "mv" [src;dst]

let remove_directory dir =
  log_command "rm" ["-rf";dir]
  
let send_file_over_ssh src dst =
  log_command "scp" [src;dst]

let log_viewer () =
  exit (Sys.command
    (sprintf "tail -f %s" (Params.get_param "session-log")))

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

;;

(* Utils *)

let scm_make_list f v =
  let rec make acc = function
    | []     -> acc
    | hd::tl ->
	make (Spair {car=(f hd); cdr=acc}) tl
  in make Snull (List.rev v)
  

(* Scheme bindings *)

let scm_prepare v =
  System.with_dir ".."
    (fun () ->
      prepare (Component.components_of_sval_array v)); Snull
      
let scm_build v =
  System.with_dir ".."
    (fun () ->
      build (Component.components_of_sval_array v)); Snull

let scm_rebuild v =
  System.with_dir ".."
    (fun () ->
      rebuild (Component.components_of_sval_array v)); Snull
      
let scm_install v =
  System.with_dir ".."
    (fun () ->
      ignore (install (Component.components_of_sval_array v))); Snull

let scm_reinstall v =
  System.with_dir ".."
    (fun () ->
      reinstall (Component.components_of_sval_array v)); Snull

let scm_simple_configure v =
  Rules.simple_configure (Scheme.string_list_of_sval_array v); Snull

let scm_simple_make v =
  Rules.simple_make (Scheme.string_list_of_sval_array v); Snull

let scm_simple_install v =
  Rules.simple_install (Scheme.string_list_of_sval_array v); Snull

let scm_export v =
  Rules.export (Scheme.make_params_of_sval v); Snull

let scm_add_path v =
  Rules.add_path (Scheme.string_list_of_sval_array v); Snull

let scm_ac_configure v =
  Rules.ac_configure (Scheme.make_params_of_sval v); Snull
    
let scm_make v =
  Rules.make (Scheme.make_params_of_sval v); Snull

let scm_path_concat v =
  Sstring (path_concat (Scheme.string_list_of_sval_array v))

let scm_string_concat v =  
  Sstring (string_concat 
    (Scheme.string_list_of_sval_array v))

let scm_log_command v =
  let l = Scheme.string_list_of_sval_array v in
  Logger.log_command (List.hd l) (List.tl l); Snull

let scm_run_command v =
  let l = Scheme.string_list_of_sval_array v in
  Sint (Logger.run_command (List.hd l) (List.tl l))

let scm_log_message v =
  let l = Scheme.string_list_of_sval_array v in
  Logger.log_message (String.concat "" l); Snull

let scm_log_error v =
  let l = Scheme.string_list_of_sval_array v in
  Logger.log_error (String.concat "" l)

let scm_install_file file dir =
  Rules.install_file
    (Scheme.string_of_sval file)
    (Scheme.string_of_sval dir);
  Snull

let scm_read_directory dir =
  scm_make_list
    (fun s -> Sstring s)
    (System.read_directory
      (Scheme.string_of_sval dir))

let scm_with_dir dir f =
  System.with_dir
    (Scheme.string_of_sval dir)
    (Scheme.unit_handler_of_sval f)

let scm_with_extension ext f files =
  System.with_extension
    (Scheme.string_of_sval ext)
    (Scheme.string_handler_of_sval f)
    (List.map Scheme.string_of_sval
      (Scheme.list_of_sval files));
  Snull

let scm_file_exists file =
  if Sys.file_exists 
    (Scheme.string_of_sval file)
  then Strue
  else Sfalse

let scm_get_env name =
  match Rules.get_env (Scheme.string_of_sval name) with
    | Some v -> Sstring v
    | None   -> Sstring ""

let scm_read_command cmd =
  scm_make_list (fun s -> Sstring s)
    (read_command (Scheme.string_of_sval cmd))

let command ?(env=Unix.environment()) ?(ignore_errors=false) ?(filter=(fun _ -> true)) command =
  let (pout,pin,perr) = Unix.open_process_full command env in
  let rec read acc ch =
    try
      let s = input_line ch in
      if filter s 
      then read (s::acc) ch
      else read acc ch
    with End_of_file -> acc in
  let outputs = String.concat "\n" (read [] pout) in
  let errors = String.concat "\n" (read [] perr) in
  let status = Unix.close_process_full (pout,pin,perr) in
  match status with
  | Unix.WEXITED st -> if ignore_errors
		       then (st, outputs, errors)
		       else failwith (sprintf "Command '%s' exited with non-nil status: %d" command st)
  | Unix.WSIGNALED signal -> failwith (sprintf "Command '%s' was killed by signal: %d" command signal)
  | Unix.WSTOPPED signal -> failwith (sprintf "Command '%s' was stopped by signal: %d" command signal)
      
let scm_command cmd =
  let (status,outputs,errors) = command (Scheme.string_of_sval cmd) in
  Spair {car = Sint status;
	 cdr =
	   Spair {car = Sstring outputs;
		  cdr=
		    Spair {car = Sstring errors;
			   cdr = Snull}}}
    
let scm_update_make_params v =
  update_make_params (Scheme.make_params_of_sval v);
  Snull

let scm_current_directory () =
  Sstring (Sys.getcwd ())

let scm_uname () =
  Sstring (List.hd (read_command "uname"))

let scm_arch () =
  Sstring (List.hd (read_command "arch"))

let scm_dirname s =
  Sstring (Filename.dirname (Scheme.string_of_sval s))

let scm_basename s =
  Sstring (Filename.basename (Scheme.string_of_sval s))

let scm_remove_file v =
  List.iter (fun file ->
    if Sys.file_exists file then
      Sys.remove file)
    (Scheme.string_list_of_sval_array v);
  Snull

let scm_move_file file dir =
  System.move_file
    (Scheme.string_of_sval file)
    (Scheme.string_of_sval dir);
  Snull

let scm_copy_file src dst =
  System.copy_file
    (Scheme.string_of_sval src)
    (Scheme.string_of_sval dst);
  Snull

let scm_make_directory v =
  make_directory 
    (Scheme.string_list_of_sval_array v);
  Snull

let scm_move_directory src dst =
  move_directory
    (Scheme.string_of_sval src)
    (Scheme.string_of_sval dst);
  Snull

let scm_remove_directory dir =
  remove_directory
    (Scheme.string_of_sval dir);
  Snull

let scm_create_symlink src dst =
  System.create_symlink
    (Scheme.string_of_sval src)
    (Scheme.string_of_sval dst);
  Snull

let scm_create_link src dst =
  System.create_link
    (Scheme.string_of_sval src)
    (Scheme.string_of_sval dst);
  Snull

let scm_is_directory s =
  if System.is_directory (Scheme.string_of_sval s)
  then Strue else Sfalse

let scm_send_file_over_ssh src dst =
  send_file_over_ssh
    (Scheme.string_of_sval src)
    (Scheme.string_of_sval dst);
  Snull

let scm_package_build_message v =
  match Scheme.string_list_of_sval_array v with
    | [host;location;pkgname;storage] ->
	Sstring 
	  (Notify.package_build_message
	    ~host ~location ~pkgname ~storage)
    | _  ->
	log_error "Invalid package-build-message usage"

let scm_send_message subj msg rcpts =
  let subject = Scheme.string_of_sval subj in
  let contents = [Scheme.string_of_sval msg] in
  let recipients =
    List.map Scheme.string_of_sval
      (Scheme.list_of_sval rcpts) in
  List.iter
    (Notify.send_message ~subject ~contents)
    recipients;
  Snull

let scm_send_html_message subj msg rcpts =
  let subject = Scheme.string_of_sval subj in
  let contents = [Scheme.string_of_sval msg] in
  let recipients =
    List.map Scheme.string_of_sval
      (Scheme.list_of_sval rcpts) in
  List.iter
    (Notify.send_message ~subject ~contents ~mimetype:"text/html; charset=UTF-8")
    recipients;
  Snull
  
let scm_write_file src dst =
  System.write_string
    ~file:(Scheme.string_of_sval src)
    ~string:(Scheme.string_of_sval dst);
  Snull

let scm_write_scheme_value src dst =
  Scheme.write_scheme_value (Scheme.string_of_sval src) dst;
  Snull
    
let scm_substring templ s =
  if Pcre.pmatch ~rex:(Pcre.regexp (Scheme.string_of_sval templ)) (Scheme.string_of_sval s)
  then Strue
  else Sfalse

let scm_substrings templ s =
  let rex = 
    Re_perl.compile_pat (Scheme.string_of_sval templ) in
  scm_make_list (fun x -> Sstring x)
    (Array.to_list (Re.get_all (Re.exec rex (Scheme.string_of_sval s))))

let scm_env_append v =
  match Scheme.string_list_of_sval_array v with
    | key::values ->	
	let cur = 
	  (try Env.find_component key with Not_found -> "") in
	let sep = 
	  (match cur with
	    | "PATH"
	    | "LD_LIBRARY_PATH" -> ":"
		(* todo *)
	    | _ -> ":")
	in
	if String.length cur = 0 then
	  Env.update key (String.concat sep values)
	else
	  Env.update key (String.concat sep (cur::values));
	Snull
    | _ -> Snull

let scm_jira_fix_issue v =
  match Scheme.string_list_of_sval_array v with
    | issue::pkg::ver::rev::[] ->
	let rev = int_of_string rev in
	Jira.fix_issue issue (pkg,ver,rev);
	Strue
    | _ -> Sfalse

let scm_git_push_cycle url depth =
  let _ = Git.git_push_cycle
    ~tags:true
    ~refspec:None
    (Scheme.string_of_sval url)
    (Scheme.make_int depth) in
  Git.git_push_cycle
    ~tags:false
    ~refspec:None
    (Scheme.string_of_sval url)
    (Scheme.make_int depth);
  Snull

let scm_git_push_tag_cycle url tag depth =
  Git.git_push_cycle
    ~tags:true
    ~refspec:(Some (Scheme.string_of_sval tag))
    (Scheme.string_of_sval url)
    (Scheme.make_int depth);
  Snull
;;

(* Register global functions *)

Ocs_env.set_pfn Scheme.env scm_prepare "prepare-components";;
Ocs_env.set_pfn Scheme.env scm_build   "build-components";;
Ocs_env.set_pfn Scheme.env scm_rebuild "rebuild-components";;
Ocs_env.set_pfn Scheme.env scm_install "install-components";;
Ocs_env.set_pfn Scheme.env scm_reinstall "reinstall-components";;

Ocs_env.set_pfn Scheme.env scm_simple_configure "simple-configure";;
Ocs_env.set_pfn Scheme.env scm_simple_make "simple-make";;
Ocs_env.set_pfn Scheme.env scm_simple_install "simple-install";;

Ocs_env.set_pf1 Scheme.env scm_export "ml-export";;
Ocs_env.set_pf1 Scheme.env scm_ac_configure "ml-ac-configure";;
Ocs_env.set_pf1 Scheme.env scm_make "ml-make";;
Ocs_env.set_pf1 Scheme.env scm_update_make_params "ml-update-make-params";;
Ocs_env.set_pfn Scheme.env scm_add_path "add-path";

Ocs_env.set_pfn Scheme.env scm_log_command "log-command";;
Ocs_env.set_pfn Scheme.env scm_run_command "run-command";;
Ocs_env.set_pfn Scheme.env scm_log_message "log-message";;
Ocs_env.set_pfn Scheme.env scm_log_error "log-error";;
Ocs_env.set_pfn Scheme.env scm_path_concat "path-concat";;
Ocs_env.set_pfn Scheme.env scm_string_concat "string-concat";;
Ocs_env.set_pf2 Scheme.env scm_install_file "install-file";;
Ocs_env.set_pf1 Scheme.env scm_read_directory "read-directory";;
Ocs_env.set_pf2 Scheme.env scm_with_dir "with-dir";;
Ocs_env.set_pf3 Scheme.env scm_with_extension "with-extension";;
Ocs_env.set_pf1 Scheme.env scm_file_exists "file-exists";;
Ocs_env.set_pf1 Scheme.env scm_get_env "get-env";;
Ocs_env.set_pf1 Scheme.env scm_read_command "read-command";;
Ocs_env.set_pf0 Scheme.env scm_current_directory "current-directory";;
Ocs_env.set_pf1 Scheme.env scm_command "command";;
  
Ocs_env.set_pf0 Scheme.env scm_uname "uname";;
Ocs_env.set_pf0 Scheme.env scm_arch "arch";;
Ocs_env.set_pf1 Scheme.env scm_dirname "dirname";;
Ocs_env.set_pf1 Scheme.env scm_basename "basename";;
Ocs_env.set_pfn Scheme.env scm_remove_file "remove-file";;
Ocs_env.set_pf2 Scheme.env scm_move_file "move-file";;
Ocs_env.set_pf2 Scheme.env scm_copy_file "copy-file";;
Ocs_env.set_pfn Scheme.env scm_make_directory "make-directory";;
Ocs_env.set_pf2 Scheme.env scm_move_directory "move-directory";;
Ocs_env.set_pf1 Scheme.env scm_remove_directory "remove-directory";;
Ocs_env.set_pf2 Scheme.env scm_create_symlink "create-symlink";;
Ocs_env.set_pf2 Scheme.env scm_create_link "create-link";;
Ocs_env.set_pf1 Scheme.env scm_is_directory "is-directory";;
Ocs_env.set_pf2 Scheme.env scm_send_file_over_ssh "send-file-over-ssh";;
Ocs_env.set_pfn Scheme.env scm_package_build_message "package-build-message";;
Ocs_env.set_pf3 Scheme.env scm_send_message "send-message";;
Ocs_env.set_pf3 Scheme.env scm_send_html_message "send-html-message";;
Ocs_env.set_pf2 Scheme.env scm_write_file "write-file";;
Ocs_env.set_pf2 Scheme.env scm_write_scheme_value "write-scheme-value";;
Ocs_env.set_pf2 Scheme.env scm_substring "substring?";;
Ocs_env.set_pf2 Scheme.env scm_substrings "substrings";;
Ocs_env.set_pfn Scheme.env scm_env_append "env-append";;

Ocs_env.set_pf2 Scheme.env scm_git_push_cycle "git-push-cycle";;
Ocs_env.set_pf3 Scheme.env scm_git_push_tag_cycle "git-push-tag-cycle";;

Ocs_env.set_pfn Scheme.env scm_jira_fix_issue "jira-fix-issue";;

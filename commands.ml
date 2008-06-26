open Git
open Rules
open Logger
open Ocs_env
open Ocs_types
open Types

let checkout_component component =
  match component.label with
    | Tag key ->
	git_checkout ~force:true ~key ()
    | Branch key -> 
	git_checkout ~force:true ~key ()
    | Current ->
	git_checkout ~force:true ~key:"master" ()

let remove_component component =
  if System.is_directory component.name then
    System.remove_directory component.name
      
let clone_component component =
  git_clone
    (Filename.concat
      (Params.get_param "git-url") component.name)
    component.name

let with_component_dir ?(strict=true) component thunk =
  let curdir = Sys.getcwd () in

  let with_dir f =
    Sys.chdir component.name; f (); thunk (); Sys.chdir curdir in

  let label_type =
    string_of_label_type component.label in
  let label = 
    string_of_label component.label in

  Params.update_param "component" component.name;
  Params.update_param "label" label;
  Params.update_param "label-type" label_type;

  log_message
    (Printf.sprintf "=> component (%s %s [%s])"
      (curdir ^ "/" ^ component.name) label_type label);

  let composite_mode =
    Params.used_composite_mode () in

  match git_worktree_status ~strict component with
    | Tree_not_exists ->
	log_message "status: working tree is not exists";
	remove_component component;
	clone_component component;
	with_dir (fun () ->
	  git_track_new_branches ();
	  checkout_component component)
    | Tree_exists_with_given_key content_status ->
	(match content_status with
	  | Tree_prepared ->
	      log_message "status: working tree is prepared";
	      with_dir (fun () -> ())
	  | Tree_changed ->
	      log_message "status: working tree is changed";
	      with_dir (fun () ->
		checkout_component component;
		git_clean ()))
    | Tree_exists_with_other_key ->
	log_message "status: working tree exists with other key";
	with_dir (fun () ->
	  checkout_component component;
	  git_clean ())
  
let non_empty_iter f = function
    []   -> log_error "don't know what to do"
  | list -> List.iter f list


(* Projects support *)

let prepare_component component =
  Printf.printf "prepare-component %s %s\n" component.name (Sys.getcwd ());
  with_component_dir ~strict:false component git_clean

let prepare components =
  non_empty_iter prepare_component components

let update_component component = (* todo: more smart implementation *)
  with_component_dir ~strict:true component
    (fun () ->      
      List.iter
      (fun branch ->
	git_checkout ~force:true ~key:branch ();
	git_clean ();
	git_pull ~refspec:branch (Filename.concat (Params.get_param "git-url") component.name))
      (git_branch ());
      git_track_new_branches ())

let update components =
  non_empty_iter update_component components

let forward_component component =
  with_component_dir ~strict:false component
    (fun () ->
      git_push (Filename.concat (Params.get_param "git-url") component.name))

let forward components =
  non_empty_iter forward_component components

let build_component_native component =
  if Sys.file_exists ".bf-build" then
    log_message (component.name ^ " already built, nothing to do")
  else
    begin
      log_message ("building " ^ component.name);
      build_rules ();
      log_message (component.name ^ " built");
      let ch = open_out ".bf-build" in
      output_string ch (string_of_float (Unix.gettimeofday ()));
      output_string ch "\n";
      close_out ch
    end

let build_component component =
  with_component_dir ~strict:false component
    (fun () ->
      build_component_native component)
	  
let build components =
  non_empty_iter build_component components

let rebuild_component component =
  let file = component.name ^ "/.bf-build" in
  if Sys.file_exists file then
    Sys.remove file;
  with_component_dir ~strict:true component
    (fun () ->
      build_component_native component)
  
let rebuild components =
  non_empty_iter rebuild_component components

let install_component component =
  with_component_dir ~strict:false component
    (fun () ->
      if Sys.file_exists ".bf-install" then
	log_message (component.name ^ " already installed, noting to do")
      else
	begin
	  if not (Sys.file_exists ".bf-build") then
	    build_component_native component;
	  log_message ("installing " ^ component.name);
	  install_rules ();
	  log_message (component.name ^ " installed");
	  let ch = open_out ".bf-install" in
	  output_string ch (string_of_float (Unix.gettimeofday ()));
	  output_string ch "\n";
	  close_out ch
	end)

let install components =
  non_empty_iter install_component components

let reinstall_component component =
  let file = component.name ^ "/.bf-install" in
  if Sys.file_exists file then
    Sys.remove file;
  install_component component

let reinstall components =
  non_empty_iter reinstall_component components

let status_component component =
  let curdir = Sys.getcwd () in
  let build =
    Sys.file_exists
      (component.name ^ "/.bf-build") in
  let install =
    Sys.file_exists
      (component.name ^ "/.bf-install") in
  let label_type =
    string_of_label_type component.label in
  let label =
    string_of_label component.label in
  let status =
    match git_worktree_status ~strict:true component with
      | Tree_not_exists -> "working tree is not exists"
      | Tree_exists_with_given_key content_status ->
	  (match content_status with
	    | Tree_prepared -> "working tree is prepared"
	    | Tree_changed  -> "working tree is changed")
      | Tree_exists_with_other_key ->
	  "working tree exists with other key" in
  log_message
    (Printf.sprintf "=> component (%s %s [%s] build:%b install:%b status: %s)"
      (curdir ^ "/" ^ component.name) label_type label build install status)

let status components =
  non_empty_iter status_component components

let tag_component tag component =
  with_component_dir ~strict:false component
    (fun () ->
      let url = 
	Filename.concat
	  (Params.get_param "git-url") component.name in
      match git_current_branch () with
	  Some branch ->
	    git_tag tag;
	    git_push ~refspec:tag url;
	    git_pull ~refspec:branch url
	| None ->
	    log_error ("cannot find current branch for " ^ component.name))

let make_tag tag components =
  non_empty_iter (tag_component tag) components

let diff_component tag_a tag_b component =
  with_component_dir ~strict:false component
    (fun () ->
      print_endline (git_diff_view ~tag_a ~tag_b))

let make_diff tag_a tag_b components =
  non_empty_iter (diff_component tag_a tag_b) components

let changelog_component buf tag_a tag_b component =
  with_component_dir ~strict:false component
    (fun () ->
      let log =
	git_log tag_a tag_b in
      if String.length log > 2 then
	begin
	  Buffer.add_string buf
	    (Printf.sprintf "\n\n\n### %s\n\n" (String.uppercase component.name));
	  Buffer.add_string buf log
	end)

let make_changelog tag_a tag_b components =
  let buf = Buffer.create 512 in
  non_empty_iter
    (changelog_component buf tag_a tag_b) components;
  Notify.send_message
    ~subject:(Printf.sprintf "bf@changelog %s -> %s" tag_a tag_b)
    ~content:(Buffer.contents buf)
    (Params.get_param "smtp-notify-email")

let components_of_composite composite =
  let composite = Rules.load_composite composite in
  let rec iter acc = function
    | Snull -> acc
    | Spair v ->
	(match v.cdr with
	  | Snull -> acc @ [Scheme.component_of_sval v.car]
	  | Spair v2 ->
	      iter (acc @ [Scheme.component_of_sval v.car]) (Spair v2)
	  | _ -> log_error "invalid composition")
    | _ -> log_error "invalid composition"
  in iter [] composite

let with_tag tag components =
  match tag with
    | None -> components
    | Some tag_name ->
	List.map
	  (fun component ->
	    { name = component.name; label = Tag tag_name})
	  components

let prepare_composite ?tag composite =
  log_message ("=> prepare-composite " ^ composite);
  prepare (with_tag tag (components_of_composite composite))

let update_composite ?tag composite =
  log_message ("=> update-composite " ^ composite);
  update (with_tag tag (components_of_composite composite))

let forward_composite ?tag composite =
  log_message ("=> forward-composite " ^ composite);
  forward (with_tag tag (components_of_composite composite))
  
let build_composite ?tag composite =
  log_message ("=> build-composite " ^ composite);
  build (with_tag tag (components_of_composite composite))

let rebuild_composite ?tag composite =
  log_message ("=> rebuild-composite " ^ composite);
  rebuild (with_tag tag (components_of_composite composite))

let install_composite ?tag composite =
  log_message ("=> install-composite " ^ composite);
  install (with_tag tag (components_of_composite composite))

let reinstall_composite ?tag composite =
  log_message ("=> reinstall-composite " ^ composite);
  reinstall (with_tag tag (components_of_composite composite))

let status_composite ?tag composite =
  log_message ("=> status-composite " ^ composite);
  status (with_tag tag (components_of_composite composite))

let tag_composite composite tag =
  log_message ("=> tag-composite " ^ composite ^ " " ^ tag);
  make_tag tag (components_of_composite composite)

let diff_composite composite tag_a tag_b =
  log_message ("=> diff-composite " ^ composite ^ " " ^ tag_a ^ ":" ^ tag_b);
  make_diff tag_a tag_b (components_of_composite composite)

let changelog_composite composite tag_a tag_b =
  log_message ("=> changelog-composite " ^ composite ^ " " ^ tag_a ^ ":" ^ tag_b);
  make_changelog tag_a tag_b (components_of_composite composite)

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
  with_dir ".."
    (fun () ->
      prepare (Scheme.components_of_sval_array v)); Snull
      
let scm_build v =
  with_dir ".."
    (fun () ->
      build (Scheme.components_of_sval_array v)); Snull

let scm_rebuild v =
  with_dir ".."
    (fun () ->
      rebuild (Scheme.components_of_sval_array v)); Snull
      
let scm_install v =
  with_dir ".."
    (fun () ->
      install (Scheme.components_of_sval_array v)); Snull

let scm_reinstall v =
  with_dir ".."
    (fun () ->
      reinstall (Scheme.components_of_sval_array v)); Snull

let scm_simple_configure v =
  Rules.simple_configure (Scheme.string_list_of_sval_array v); Snull

let scm_simple_make v =
  Rules.simple_make (Scheme.string_list_of_sval_array v); Snull

let scm_simple_install v =
  Rules.simple_install (Scheme.string_list_of_sval_array v); Snull

let scm_export v =
  Rules.export (Scheme.make_params_of_sval v); Snull

let scm_ac_configure v =
  Rules.ac_configure (Scheme.make_params_of_sval v); Snull
    
let scm_make v =
  Rules.make (Scheme.make_params_of_sval v); Snull

let scm_path_concat v =
  Sstring (Rules.path_concat (Scheme.string_list_of_sval_array v))

let scm_string_concat v =  
  Sstring (Rules.string_concat 
    (Scheme.string_list_of_sval_array v))

let scm_log_command v =
  let l = Scheme.string_list_of_sval_array v in
  Logger.log_command (List.hd l) (List.tl l); Snull

let scm_install_file file dir =
  Rules.install_file
    (Scheme.string_of_sval file)
    (Scheme.string_of_sval dir);
  Snull

let scm_read_directory dir =
  scm_make_list
    (fun s -> Sstring s)
    (Rules.read_directory
      (Scheme.string_of_sval dir))

let scm_with_dir dir f =
  Rules.with_dir
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
    (Rules.read_command (Scheme.string_of_sval cmd))

let scm_update_make_params v =
  Rules.update_make_params (Scheme.make_params_of_sval v);
  Snull

let scm_current_directory () =
  Sstring (Sys.getcwd ())

let scm_uname () =
  Sstring (List.hd (Rules.read_command "uname"))

let scm_arch () =
  Sstring (List.hd (Rules.read_command "arch"))

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
  Rules.move_file 
    (Scheme.string_of_sval file)
    (Scheme.string_of_sval dir);
  Snull

let scm_make_directory v =
  Rules.make_directory 
    (Scheme.string_list_of_sval_array v);
  Snull

let scm_move_directory src dst =
  Rules.move_directory
    (Scheme.string_of_sval src)
    (Scheme.string_of_sval dst);
  Snull

let scm_remove_directory dir =
  Rules.remove_directory
    (Scheme.string_of_sval dir);
  Snull

let scm_create_symlink src dst =
  Rules.create_symlink
    (Scheme.string_of_sval src)
    (Scheme.string_of_sval dst);
  Snull

let scm_create_link src dst =
  Rules.create_link
    (Scheme.string_of_sval src)
    (Scheme.string_of_sval dst);
  Snull

let scm_is_directory s =
  if System.is_directory (Scheme.string_of_sval s)
  then Strue else Sfalse

let scm_send_file_over_ssh src dst =
  Rules.send_file_over_ssh
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
  let content = Scheme.string_of_sval msg in
  let recipients =
    List.map Scheme.string_of_sval
      (Scheme.list_of_sval rcpts) in
  List.iter
    (Notify.send_message ~subject ~content)
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
    
;;

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

Ocs_env.set_pfn Scheme.env scm_log_command "log-command";;
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

Ocs_env.set_pf0 Scheme.env scm_uname "uname";;
Ocs_env.set_pf0 Scheme.env scm_arch "arch";;
Ocs_env.set_pf1 Scheme.env scm_dirname "dirname";;
Ocs_env.set_pf1 Scheme.env scm_basename "basename";;
Ocs_env.set_pfn Scheme.env scm_remove_file "remove-file";;
Ocs_env.set_pf2 Scheme.env scm_move_file "move-file";;
Ocs_env.set_pfn Scheme.env scm_make_directory "make-directory";;
Ocs_env.set_pf2 Scheme.env scm_move_directory "move-directory";;
Ocs_env.set_pf1 Scheme.env scm_remove_directory "remove-directory";;
Ocs_env.set_pf2 Scheme.env scm_create_symlink "create-symlink";;
Ocs_env.set_pf2 Scheme.env scm_create_link "create-link";;
Ocs_env.set_pf1 Scheme.env scm_is_directory "is-directory";;
Ocs_env.set_pf2 Scheme.env scm_send_file_over_ssh "send-file-over-ssh";;
Ocs_env.set_pfn Scheme.env scm_package_build_message "package-build-message";;
Ocs_env.set_pf3 Scheme.env scm_send_message "send-message";;
Ocs_env.set_pf2 Scheme.env scm_write_file "write-file";;
Ocs_env.set_pf2 Scheme.env scm_write_scheme_value "write-scheme-value";;



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
	git_checkout ~force:true ()

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
	log_message "status: working tree not exists";
	remove_component component;
	clone_component component;
	with_dir (fun () ->
	  git_track_new_branches ();
	  checkout_component component)
    | Tree_exists_with_given_key content_status ->
	(match content_status with
	  | Tree_prepared ->
	      log_message "status: working tree prepared";
	      with_dir (fun () -> ())
	  | Tree_changed ->
	      log_message "status: working tree changed";
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

let update_component component =
  with_component_dir ~strict:true component
    (fun () ->
      git_pull (Filename.concat (Params.get_param "git-url") component.name);
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

let prepare_composite composite =
  log_message ("=> prepare-composite " ^ composite);
  prepare (components_of_composite composite)

let update_composite composite =
  log_message ("=> update-composite " ^ composite);
  update (components_of_composite composite)

let forward_composite composite =
  log_message ("=> forward-composite " ^ composite);
  forward (components_of_composite composite)
  
let build_composite composite =
  log_message ("=> build-composite " ^ composite);
  build (components_of_composite composite)

let rebuild_composite composite =
  log_message ("=> rebuild-composite " ^ composite);
  rebuild (components_of_composite composite)

let install_composite composite =
  log_message ("=> install-composite " ^ composite);
  install (components_of_composite composite)

let reinstall_composite composite =
  log_message ("=> reinstall-composite " ^ composite);
  reinstall (components_of_composite composite)
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
Ocs_env.set_pfn Scheme.env scm_remove_file "remove-file";;
Ocs_env.set_pf2 Scheme.env scm_move_file "move-file";;
Ocs_env.set_pfn Scheme.env scm_make_directory "make-directory";;
Ocs_env.set_pf2 Scheme.env scm_move_directory "move-directory";;
Ocs_env.set_pf1 Scheme.env scm_remove_directory "remove-directory";;
Ocs_env.set_pf2 Scheme.env scm_create_symlink "create-symlink";;
Ocs_env.set_pf2 Scheme.env scm_create_link "create-link";;





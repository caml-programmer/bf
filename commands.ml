open Git
open Rules
open Logger
open Ocs_env
open Ocs_types
open Types

type analyze_result =
  | Worktree_conflicted
  | Worktree_prepared
  | Worktree_need_checkout
  | Worktree_need_recreate
  | Worktree_need_create

let key_status component =
  let cur = Sys.getcwd () in
  Sys.chdir component.name;
  let status =
    git_key_status component
  in Sys.chdir cur; status

let worktree_analyze component =
  let composite_mode =
    Params.used_composite_mode () in
  
  match git_tree_status component with
    | Exists ->
	(* but different -> checkout all or recreate *)
	if composite_mode then
	  Worktree_need_recreate
	else
	  Worktree_conflicted
    | Not_exists ->
	(* clone, checkout -> create *)
	Worktree_need_create
    | Be_set ->
	(* check tag && branch if needed *)
	(match key_status component with
	  | Exists ->
	      if composite_mode then
		Worktree_need_recreate (* todo: stave off loop *)
	      else
		Worktree_need_checkout
	  | Not_exists ->
	      Worktree_need_recreate
	  | Be_set ->
	      Worktree_prepared)

let checkout_component component =
  match component.label with
    | Tag key ->
	git_checkout ~key ()
    | Branch key -> 
	git_checkout ~key ()
    | Current ->
	git_checkout ()

let remove_component component =
  if System.is_directory component.name then
    System.remove_directory component.name
      
let clone_component component =
  git_clone
    (Filename.concat
      (Params.get_param "git-url") component.name)

let with_component_dir component thunk =
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
    (Printf.sprintf "=> with-component-dir(%s %s [%s])"
      (curdir ^ "/" ^ component.name) label_type label);
  
  match worktree_analyze component with
    | Worktree_conflicted ->
	log_error (Printf.sprintf "=> component %s is conflicted" component.name)
    | Worktree_prepared ->
	with_dir (fun () -> ())
    | Worktree_need_checkout ->
	with_dir (fun () -> checkout_component component)
    | Worktree_need_recreate ->
	remove_component component;
	clone_component component;
	with_dir (fun () -> checkout_component component)
    | Worktree_need_create ->
	clone_component component;
	with_dir (fun () -> checkout_component component)

let non_empty_iter f = function
    []   -> log_error "don't know what to do"
  | list -> List.iter f list


(* Projects support *)

let prepare_component component =
  with_component_dir component git_clean

let prepare components =
  non_empty_iter prepare_component components

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
  with_component_dir component
    (fun () ->
      build_component_native component)
	  
let build components =
  non_empty_iter build_component components

let rebuild_component component =
  let bf_build =
    Filename.concat component.name ".bf-build" in
  if Sys.file_exists bf_build then
    Sys.remove bf_build;
  build_component component
	  
let rebuild components =
  non_empty_iter rebuild_component components

let install_component component =
  with_component_dir component
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

let components_of_composite composite =
  let composite = Rules.load_composite composite in
  let rec iter acc = function
    | Snull -> acc
    | Spair v ->
	(match v.cdr with
	  | Snull -> acc
	  | Spair v ->
	      iter (acc @ [Scheme.component_of_sval v.car]) (Spair v)
	  | _ -> log_error "invalid composition")
    | _ -> log_error "invalid composition"
  in iter [] composite

let prepare_composite composite =
  log_message ("=> prepare-composite " ^ composite);
  prepare (components_of_composite composite)
  
let build_composite composite =
  log_message ("=> build-composite " ^ composite);
  build (components_of_composite composite)

let rebuild_composite composite =
  log_message ("=> rebuild-composite " ^ composite);
  rebuild (components_of_composite composite)

let install_composite composite =
  log_message ("=> install-composite " ^ composite);
  install (components_of_composite composite)
;;

(* Components support *)

let simple_configure args =
  log_command "./configure" args
 
let simple_make args =
  log_command "make" args

let simple_install args =
  log_command "make" ("install"::args)
  

(* Scheme bindings *)

let scm_prepare v =
  prepare (Scheme.components_of_sval_array v); Snull

let scm_build v =
  build (Scheme.components_of_sval_array v); Snull

let scm_rebuild v =
  rebuild (Scheme.components_of_sval_array v); Snull

let scm_install v =
  install (Scheme.components_of_sval_array v); Snull

let scm_simple_configure v =
  simple_configure (Scheme.string_list_of_sval_array v); Snull

let scm_simple_make v =
  simple_make (Scheme.string_list_of_sval_array v); Snull

let scm_simple_install v =
  simple_install (Scheme.string_list_of_sval_array v); Snull

;;

Ocs_env.set_pfn Scheme.env scm_prepare "prepare";;
Ocs_env.set_pfn Scheme.env scm_build   "build";;
Ocs_env.set_pfn Scheme.env scm_rebuild "rebuild";;
Ocs_env.set_pfn Scheme.env scm_install "install";;

Ocs_env.set_pfn Scheme.env scm_simple_configure "simple-configure";;
Ocs_env.set_pfn Scheme.env scm_simple_make "simple-make";;
Ocs_env.set_pfn Scheme.env scm_simple_install "simple-install";;

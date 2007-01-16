open Git
open Rules
open Logger
open Ocs_env
open Ocs_types

exception Checkout_done

let with_component_dir component thunk =
  let curdir = Sys.getcwd () in
  if not (Sys.file_exists component) then
    begin
      git_clone ((Params.get_param "git_url") ^ "/" ^ component);
      Sys.chdir component;
      let uname = System.uname () in
      let branches = git_branch () in
      let select = [uname;"jet";"master"] in
      (try
	List.iter
	  (fun branch ->
	    if List.mem branch branches then
	      begin
		git_checkout ~name:branch ();
		raise Checkout_done
	      end)
	  branches;
	git_checkout ();
      with Checkout_done -> ());
      thunk ();
      Sys.chdir curdir
    end
  else
    begin 
      Sys.chdir component;
      thunk ();
      Sys.chdir curdir 
    end

let non_empty_iter f = function
    []   -> log_error "don't know wat to do"
  | list -> List.iter f list

let prepare_component component =
  with_component_dir component git_clean

let prepare components =
  non_empty_iter prepare_component components

let build_component component =
  with_component_dir component
    (fun () ->
      if Sys.file_exists ".bf-build" then
	log_message (component ^ " already built, nothing to do")
      else
	log_message ("building " ^ component);
	build_rules ();
	log_message (component ^ " built");
	let ch = open_out ".bf-build" in
	output_string ch (string_of_float (Unix.gettimeofday ()));
	output_string ch "\n";
	close_out ch)
	  
let build components =
  non_empty_iter build_component components

let rebuild_component component =
  with_component_dir component
    (fun () ->
      if Sys.file_exists ".bf-build" then
	Sys.remove ".bf-build");
  build_component component
	  
let rebuild components =
  non_empty_iter rebuild_component components

let install_component component =
  with_component_dir component
    (fun () ->
      if Sys.file_exists ".bf-install" then
	log_message (component ^ "already installed, noting to do")
      else
	log_message ("installing " ^ component);
	install_rules ();
	log_message (component ^ " installed");
	let ch = open_out ".bf-install" in
	output_string ch (string_of_float (Unix.gettimeofday ()));
	output_string ch "\n";
	close_out ch)

let install components =
  non_empty_iter install_component components


(* Scheme bindings *)

let scm_prepare v =
  prepare (Scheme.string_list_of_sval_array v); Snull

let scm_build v =
  build (Scheme.string_list_of_sval_array v); Snull

let scm_rebuild v =
  rebuild (Scheme.string_list_of_sval_array v); Snull

let scm_install v =
  install (Scheme.string_list_of_sval_array v); Snull

;;

Ocs_env.set_pfn Scheme.env scm_prepare "prepare";;
Ocs_env.set_pfn Scheme.env scm_build   "build";;
Ocs_env.set_pfn Scheme.env scm_rebuild "rebuild";;
Ocs_env.set_pfn Scheme.env scm_install "install";;


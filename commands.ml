open Git
open Params
open Rules

exception Checkout_done

let with_component_dir component thunk =
  let curdir = Sys.getcwd () in
  if not (Sys.file_exists component) then
    begin
      git_clone (git_url ^ "/" ^ component);
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
    []   -> error "don't know wat to do"
  | list -> List.iter f list

let prepare_component component =
  with_component_dir component git_clean

let prepare components =
  non_emprt_iter prepare_component components

let build_component component =
  with_component_dir component
    (fun () ->
      if Sys.file_exists ".bf-build" then
	log_message (component ^ " already built, nothing to do")
      else
	let build = build_rules () in
	log_message ("building " ^ component);
	build ();
	log_message (component ^ " built");
	let ch = open_out ".bf-build" in
	output_string ch (string_of_float (Unix.gettimeofday ()));
	output_string ch "\n";
	close_out ch)
	  
let build components =
  non_emprt_iter build_component components

let rebuild_component component =
  with_component_dir component
    (fun () ->
      if Sys.file_exists ".bf-build" then
	Sys.remove ".bf-build");
  build_component component
	  
let rebuild components =
  non_emprt_iter rebuild_component components

let install_component component =
  with_component_dir component
    (fun () ->
      if Sys.file_exists ".bf-install" then
	log_message (component ^ "already installed, noting to do")
      else
	let install = install_rules () in
	log_message ("installing " ^ component);
	install ();
	log_message (component ^ " installed");
	let ch = open_out ".bf-install" in
	output_string ch (string_of_float (Unix.gettimeofday ()));
	output_string ch "\n";
	close_out ch)

let install components =
  non_empty_iter install_component components

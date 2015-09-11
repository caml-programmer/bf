open System
open Git
open Logger
open Printf
open Scheme
open Ocs_types
       
exception Bad_forkmode of string

type label = Tag of string | Branch of string | Current
			    
type forkmode =
  | Tagging
  | Branching
  | Inherit

type component = {
  name  : string;
  label : label;
  pkg : string option;
  rules : string option;
  nopack: bool;
  forkmode : forkmode;
}

type content_status =
  | Tree_prepared               (* nothing to do *)
  | Tree_changed of string list (* checkout -f && clean -d *)

type worktree_status =
  | Tree_not_exists            (* do remove, clone and checkout -f *)
  | Tree_exists_with_given_key of content_status
  | Tree_exists_with_other_key of string (* do checkout -f and clean -d *)

type version = string
type revision = int

let string_of_label = function
  | Tag s -> s
  | Branch s -> s
  | Current -> ""

let forkmode_of_string = function
  | "do-branch" | "branch" | "branching" -> Branching
  | "fixtag" | "tag" | "tagging"         -> Tagging
  | "inherit"                            -> Inherit
  | x -> raise (Bad_forkmode x)

let string_of_forkmode = function
  | Tagging -> "tagging"
  | Branching -> "branching"
  | Inherit -> "inherit"

let string_of_label_type = function
  | Tag _    -> "tag"
  | Branch _ -> "branch"
  | Current  -> "current"

let string_of_rules = function
  | None -> ""
  | Some r -> r

let string_of_string_option = function
  | Some (x:string) -> x
  | None -> ""

let string_of_component comp = 
  String.concat "\n"
    [
      (Printf.sprintf "NAME: %s" comp.name);
      (Printf.sprintf "LABEL: %s %s" (string_of_label_type comp.label)
		      (string_of_label comp.label));
      (Printf.sprintf "PKG: %s" (string_of_string_option comp.pkg));
      (Printf.sprintf "RULES: %s" (string_of_string_option comp.rules));
      (Printf.sprintf "NOPACK: %B" comp.nopack);
      (Printf.sprintf "FORKMODE: %s" (string_of_forkmode comp.forkmode));
    ]



let git_worktree_status ~strict component =
  let ignore =
    if Sys.file_exists ".bf-ignore" then
      list_of_channel (open_in ".bf-ignore")
    else []
  in  
  let worktree_changes =
    if not strict then [] else git_status () in
  let source_changes =
    match component.label with
      | Current ->
	  git_diff ~ignore ()
      | Tag key ->
	  git_diff ~ignore ~key ()
      | Branch key ->
	  git_diff ~ignore ~key ()
  in
  if source_changes = [] && worktree_changes = [] then
    Tree_prepared
  else
    Tree_changed (source_changes @ worktree_changes)

let git_tag_status ~strict component =
  match component.label with
    | Current  -> assert false
    | Branch _ -> assert false
    | Tag m ->
	let tags = git_tag_list () in
	if not (List.mem m tags) then
	  Tree_exists_with_other_key "unknown"
	else
	  Tree_exists_with_given_key 
	    (git_worktree_status ~strict component)

let git_branch_status ~strict component =
  match component.label with
    | Current  -> assert false
    | Tag _ -> assert false
    | Branch m ->
	match git_current_branch () with
	  | Some cur ->
	      if cur = m then
		Tree_exists_with_given_key (git_worktree_status ~strict component)
	      else
		Tree_exists_with_other_key cur
	  | None ->
	      Tree_exists_with_other_key "unknown"

let git_key_status ~strict component =
  match component.label with
    | Current  -> Tree_exists_with_given_key (git_worktree_status ~strict component)
    | Tag _    -> git_tag_status ~strict component
    | Branch _ -> git_branch_status ~strict component

let git_component_status ~strict component =
  let cur = Sys.getcwd () in
  if not (System.is_directory component.name) then
    Tree_not_exists
  else
    begin
      Sys.chdir component.name;
      let status =
	git_key_status ~strict component in
      Sys.chdir cur;
      status
    end

let git_create_url component =
  let s = Params.get_param "git-url" in
  match Pcre.split (Re.compile (Re.rep1 Re.space)) s with
    | [] -> raise Invalid_url
    | [one] -> one
    | list ->
	try
	  List.iter
	    (fun url ->
	      check_component_url url component.name)
	    list;
	  raise (Component_not_found component.name)
	with Found_component url -> url




    



       
let with_rules s c =
  match c.rules with
    | Some alt ->
	s ^ "." ^ alt
    | None -> s

let make ?(label=Current) ?(pkg=None) ?(rules=None) ?(nopack=false) ?(forkmode=Branching) s =
  { name = s; label = label; pkg = pkg; rules = rules; nopack = nopack; forkmode = forkmode }

let checkout ?(low=false) component =
  match component.label with
    | Tag key ->
	git_checkout ~low ~force:true ~key ()
    | Branch key ->
	git_checkout ~low ~force:true ~key ()
    | Current ->
	let branches =
	  git_branch ~remote:false () in
	if List.mem "master" branches then
	  git_checkout ~low ~force:true ~key:"master" ()
	else
	  git_checkout ~low ~force:true ~key:"HEAD" ()

let remove component =
  if System.is_directory component.name then
    System.remove_directory component.name
      
let clone component =
  git_clone
    (Filename.concat 
      (git_create_url component) component.name) component.name

let with_component_dir ?(low=false) ?(strict=true) component (thunk : unit -> unit) =
  let curdir = Sys.getcwd () in

  let with_dir f =
    Sys.chdir component.name;
    let with_changes = f () in
    try
      thunk ();
      Sys.chdir curdir;
      with_changes
    with exn ->
      Sys.chdir curdir;
      raise exn
  in

  let label_type =
    string_of_label_type component.label in
  let label = 
    string_of_label component.label in

  Params.update_param "component" component.name;
  Params.update_param "label" label;
  Params.update_param "label-type" label_type;

  log_message ~low
    (Printf.sprintf "=> component (%s %s [%s])"
      (curdir ^ "/" ^ component.name) label_type label);

  match git_component_status ~strict component with
    | Tree_not_exists ->
	log_message ~low "status: working tree is not exists";
	remove component;
	clone component;
	with_dir (fun () ->
	  checkout ~low component;
	  true)
    | Tree_exists_with_given_key content_status ->
	(match content_status with
	  | Tree_prepared ->
	      log_message ~low "status: working tree is prepared";
	      with_dir (fun () -> false)
	  | Tree_changed changes ->
	      log_message ~low
		(sprintf "status: working tree is changed(%d)" (List.length changes));
	      with_dir (fun () ->
		checkout ~low component; git_clean ();
		true))
    | Tree_exists_with_other_key key ->
	log_message ~low 
	  (sprintf "status: working tree exists with other key (%s)" key);
	with_dir (fun () ->
	  (match component.label with
	    | Branch b ->
		let rb = git_branch ~remote:true () in
		let lb = git_branch ~remote:false () in
		if not (List.mem b lb) then
		  begin
		    if not (List.mem (Branch.origin b) rb) then
		      begin
			git_remote_update "origin";
		      end;
		  end
	    | _ -> ());	  
	  checkout ~low component;
	  git_clean ();
	  true)

let prepare component =
  log_message (sprintf "prepare-component %s %s\n" component.name (Sys.getcwd ()));
  ignore (with_component_dir ~strict:false component git_clean)

let forward component =
  ignore 
    (with_component_dir ~strict:false component
      (fun () ->
	git_push (Filename.concat (git_create_url component) component.name)))

let build_native ?(snapshot=false) component =
  if Sys.file_exists (with_rules ".bf-build" component) then
    log_message (component.name ^ " already built, nothing to do")
  else
    begin
      let t0 = Unix.gettimeofday () in
      log_message (sprintf "build %s started" (with_rules component.name component));
      Params.update_param "install-dir" (Params.make_install_dir ());
      Rules.build_rules ~snapshot component.rules;
      log_message (component.name ^ " built");
      let t1 = Unix.gettimeofday () in
      let ch = open_out (with_rules ".bf-build" component) in
      output_string ch (string_of_float (Unix.gettimeofday ()));
      output_string ch "\n";
      output_string ch (string_of_float (t1 -. t0));
      output_string ch "\n";
      close_out ch;
      log_message (sprintf "build %s finished (%f seconds)" (with_rules component.name component) (t1 -. t0));
    end

let build component =
  ignore 
    (with_component_dir ~strict:true component
      (fun () ->
	build_native component))

let rebuild component =
  let file =
    sprintf "%s/%s" component.name
      (with_rules ".bf-build" component) in
  if Sys.file_exists file then
    Sys.remove file;
  ignore (with_component_dir ~strict:true component
    (fun () ->
      build_native component))
  
let install ?(snapshot=false) component =
  match component.label, component.pkg with
    | Tag _, Some pkg ->
	log_message
	  (component.name ^ (sprintf " must be installed by package (%s), nothing to do" pkg));
	false
    | _ ->
	let result = ref false in
	ignore
	  (with_component_dir ~strict:false component
	    (fun () ->
	      if Sys.file_exists (with_rules ".bf-install" component)
		&& Sys.file_exists (with_rules ".bf-build" component) then
		  log_message ((with_rules component.name component) ^ " already installed, nothing to do")
	      else
		begin
		  if not (Sys.file_exists (with_rules ".bf-build" component)) then
		    build_native ~snapshot component;
		  let t0 = Unix.gettimeofday () in
		  log_message ("install " ^ component.name ^ " started");
		  let top_dir =
		    Params.get_param "top-dir" in
		  let dest_dir =
		    Params.get_param "dest-dir" in
		  let install_dir =
		    Params.make_install_dir () in
		  let state =
		    Scanner.create_top_state install_dir in
		  Params.update_param "orig-top-dir" top_dir;
		  Params.update_param "install-dir" install_dir;
		  if dest_dir <> "" then
		    begin
		      Params.update_param "top-dir" install_dir; (* Deprecated: use install-dir *)
		    end;
		  Rules.install_rules ~snapshot component.rules;
		  Params.update_param "top-dir" top_dir;
		  Scanner.generate_changes component.rules top_dir
		    state (Scanner.create_top_state install_dir);
		  let t1 = Unix.gettimeofday () in
		  log_message ("install " ^ component.name ^ (sprintf " finished (%f seconds)" (t1 -. t0)));
		  let ch = open_out (with_rules ".bf-install" component) in
		  output_string ch (string_of_float (Unix.gettimeofday ()));
		  output_string ch "\n";
		  output_string ch (string_of_float (t1 -. t0));
		  output_string ch "\n";
		  close_out ch;
		  result := true;
		end));
	!result

let update ?(snapshot=false) component =
  let exists s =
    Sys.file_exists (Filename.concat component.name s) in
  let status_changes =
    not (exists (with_rules ".bf-build" component)) ||
    not (exists (with_rules ".bf-install" component)) in
  let remote_changes = ref false in
  let local_changes =
    with_component_dir ~strict:false component
      (fun () ->
	let start = git_current_branch () in
	git_remote_update "origin";
	let update branch =
	  if git_changed branch (Branch.origin branch) then
	    begin
	      git_checkout ~force:true ~key:branch ();
	      git_clean ();
	      git_merge (Branch.origin branch);
	      remote_changes := true
	    end
	in
	List.iter
	  (fun branch ->	    
	    (match component.label with
	      | Branch b ->
		  if branch = b then
		    update branch
	      | Tag _ -> ()
	      | Current ->
		  (match start with
		    | None ->
			update branch
		    | Some s ->
			if branch = s then
			  update branch)))
	  (git_branch ());
	let stop = git_current_branch () in
	match start, stop with
	  | Some start_key, Some stop_key -> 
	      if start_key <> stop_key then
		git_checkout ~force:true ~key:start_key ()
	  | Some start_key, None ->
	      git_checkout ~force:true ~key:start_key ()
	  | None, _ -> ())
  in 
  if component.name <> Params.get_param "pack" then
    begin
      log_message (sprintf "changes in component %s: local-changes(%b), remote-changes(%b), status-changes(%b)" component.name local_changes !remote_changes status_changes);
      local_changes || !remote_changes || status_changes
    end
  else false

let reinstall component =
  let file =
    sprintf "%s/%s" component.name
      (with_rules ".bf-install" component) in
  if Sys.file_exists file then
    Sys.remove file;
  ignore(install component)

let fetch_tags component =
  ignore
    (with_component_dir ~strict:false component
      (fun () ->
	git_remote_update "origin";
	git_track_new_branches ()))

let status ?(max_component_length=0) ?(max_label_length=0) component =
  let build =
    Sys.file_exists
      (sprintf "%s/%s" component.name (with_rules ".bf-build" component)) in
  let install =
    Sys.file_exists
      (sprintf "%s/%s" component.name (with_rules ".bf-install" component)) in
  let label_type =
    string_of_label_type component.label in
  let label =
    string_of_label component.label in
  let composite_mode =
    Params.used_composite_mode () in
  let changes = ref [] in
  let status =
    match git_component_status ~strict:false component with
      | Tree_not_exists -> "working tree is not exists"
      | Tree_exists_with_given_key content_status ->
	  (match content_status with
	    | Tree_prepared -> "working tree is prepared"
	    | Tree_changed l -> changes := l;
		(sprintf "working tree is changed (%d)" (List.length l)))
      | Tree_exists_with_other_key key ->
	  (sprintf "working tree exists with other key (%s)" key) in
  let make_suffix max s c =
    if max = 0 then " " else String.make (2 + (max - (String.length s))) c in

  let component_suffix =
    make_suffix max_component_length component.name '.' in
  let label_suffix =
    make_suffix max_label_length label '_' in
  let label_type_suffix =
    make_suffix 7 label_type ' ' in

  log_message
    (Printf.sprintf "=> component (%s%s%s%s[%s]%s b:%b\ti:%b\ts: %s)"
      component.name component_suffix label_type label_type_suffix label label_suffix build install status);
  if not composite_mode then
    List.iter log_message !changes

let tag tag component =
  let call () =
    let url = 
      Filename.concat
	(git_create_url component) component.name in
    if git_current_branch () = None then
      log_message ("Warning: cannot find current branch for " ^ component.name);
    match git_make_tag tag with
      | Tag_created | Tag_already_exists ->
	  git_push_cycle ~tags:false ~refspec:(Some tag) url 10
      | Tag_creation_problem -> raise Logger.Error
  in ignore
       (with_component_dir ~strict:false component call)

let update_pack component =
  let remote_changes = ref false in
  let local_changes =
    with_component_dir ~strict:true component
      (fun () ->
	let start = git_current_branch () in
	git_remote_update "origin";
	git_track_new_branches ();
	List.iter
	  (fun branch ->
	    if git_changed branch (Branch.origin branch) then
	      begin
		git_checkout ~force:true ~key:branch ();
		git_clean ();
		git_merge (Branch.origin branch);
		remote_changes := true
	      end)
	  (git_branch ());
	let stop = git_current_branch () in
	match start, stop with
	  | Some start_key, Some stop_key ->
	      if start_key <> stop_key then
		git_checkout ~force:true ~key:start_key ()
	  | Some start_key, None ->
	      git_checkout ~force:true ~key:start_key ()
	  | None, _ -> ())
  in ignore
       (install component); (* need for create .bf-build and .bf-install *)
  (local_changes || !remote_changes)

let infostring component =
  sprintf "%s (%s) (%s)" component.name (string_of_label_type component.label) (string_of_label component.label)

let diff tag_a tag_b component =
  ignore (with_component_dir ~strict:false component
    (fun () ->
      print_endline (git_diff_view ~tag_a ~tag_b)))

let changelog ?(branch=None) ?(diff=false) ?(since=None) tag_a tag_b component =
  let chunks = ref [] in
  ignore (with_component_dir ~low:true ~strict:false component
    (fun () ->
      
      Git.git_fetch ~tags:true ();

      let pack =
	if component.name = Params.get_param "pack" then
	  match branch with
	    | None ->
		Some (Filename.dirname tag_a)
	    | Some b ->
		Some (Filename.concat (Filename.dirname tag_a) b)
	else
	  None
      in
      let logs = git_log ~pack ~diff ~since tag_a tag_b in
      if List.length logs > 0 && String.length (List.nth logs 0) > 2
      then
	begin	
	  let chunk =
	    Printf.sprintf "\n %s (%s) (%s)\n"
	      (String.uppercase component.name)
	      (string_of_label_type component.label)
	      (string_of_label component.label) in
	  let clearlogs =
	    List.filter
	      (fun log -> 
		(*(not (Strings.substring_exists "update dependencies" log)) &&*)
		(not (Strings.substring_exists "[bf] prepare" log)))
	      logs in
	  chunks := chunk::clearlogs
	end));
  !chunks

let extract_tasks s =
  let comma = Str.regexp "," in
  let rex =
    Re_perl.compile_pat "[^A-Z]*([A-Z]+-\\d+)[^0-9]*" in
  List.fold_left
    (fun acc s ->
      let a = Re.get_all (Re.exec rex s) in
      if Array.length a > 1 then
	a.(1)::acc
      else acc)
    [] (Str.split comma s)

let changelog_tasks ?(branch=None) ?(diff=false) ?(since=None) tag_a tag_b component =
  let tasks = ref [] in
  ignore (with_component_dir ~low:true ~strict:false component
    (fun () ->
      
      Git.git_fetch ~tags:true ();

      let pack =
	if component.name = Params.get_param "pack" then
	  match branch with
	    | None ->
		Some (Filename.dirname tag_a)
	    | Some b ->
		Some (Filename.concat (Filename.dirname tag_a) b)
	else
	  None
      in
      let logs = git_log ~pack ~diff ~since tag_a tag_b in
      if List.length logs > 0 && String.length (List.nth logs 0) > 2 then
	begin
	  List.iter 
	    (fun log ->
	      tasks := (extract_tasks log) @ !tasks)
	    logs	    
	end));
  !tasks


exception Nopack_mode_conflict_with_package of string

let component_of_sval s =
  match s with
    | Spair v ->
	let name =
	  (match v.car with
	    | Ssymbol s -> s
	    | x -> error x)
	in
	let pkg = ref None in
	let rules = ref None in
	let label = ref Current in
	let nopack = ref false in
	let forkmode = ref Branching in
	let rec scan = function
	  | Spair v ->
	      (match v.car with
		| Spair x ->
		    (match x.car with
		      | Ssymbol "branch" ->
			  label := Branch (match fst x.cdr with Sstring s -> s | x -> error x)
		      | Ssymbol "tag" ->
			  label := Tag (match fst x.cdr with Sstring s ->  s | x -> error x)
		      | Ssymbol "package" ->
			  pkg := Some (match fst x.cdr with Sstring s ->  s | x -> error x)
		      | Ssymbol "rules" ->
			  rules := Some (match fst x.cdr with Sstring s ->  s | x -> error x)
		      | Ssymbol "nopack" ->
			  (match !pkg with
			    | None   -> nopack := true;
			    | Some s ->
				raise (Nopack_mode_conflict_with_package s))
		      | Ssymbol "fork"
		      | Ssymbol "on-fork" ->
			  forkmode := forkmode_of_string
			    (match fst x.cdr with
			      | Ssymbol s -> s
			      | Sstring s -> s
			      | x -> error x)
		      | x -> error x)
		| Snull -> ()
		| x -> error x);
	      scan v.cdr;
	  | Snull -> ()
	  | _ -> assert false
	in scan v.cdr;
	{ 
	  name = name; 
	  label = !label;
	  pkg = !pkg;
	  rules = !rules;
	  nopack = !nopack;
	  forkmode = !forkmode;
	}
    | x -> error x
 
let components_of_sval_array v =
  List.map component_of_sval (Array.to_list v)



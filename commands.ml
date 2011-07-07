open Git
open Rules
open Logger
open Ocs_types
open Printf
open Types

let checkout_component ?(low=false) component =
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

let remove_component component =
  if System.is_directory component.name then
    System.remove_directory component.name
      
let clone_component component =
  git_clone
    (Filename.concat 
      (git_create_url component) component.name) component.name

let origin s = 
  "origin/" ^ s

let with_rules s c =
  match c.rules with
    | Some alt ->
	s ^ "." ^ alt
    | None -> s

let with_component_dir ?(low=false) ?(strict=true) component (thunk : unit -> unit) =
  let curdir = Sys.getcwd () in

  let with_dir f =
    Sys.chdir component.name;
    let with_changes = f () in
    thunk ();
    Sys.chdir curdir;
    with_changes
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
	remove_component component;
	clone_component component;
	with_dir (fun () ->
	  git_track_new_branches ();
	  checkout_component ~low component;
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
		checkout_component ~low component; git_clean ();
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
		    if not (List.mem (origin b) rb) then
		      begin
			git_fetch "origin";
			git_fetch ~tags:true "origin";		     
		      end;
		    git_track_new_branches ();
		  end
	    | _ -> ());	  
	  checkout_component ~low component;
	  git_clean ();
	  true)
  
let non_empty_iter f = function
    []   -> log_error "don't know what to do"
  | list -> List.iter f list

let non_empty_map f = function
    []   -> log_error "don't know what to do"
  | list -> List.map f list

let non_empty_list = function
    []   -> log_error "don't know what to do"
  | list -> list


(* Projects support *)

let prepare_component component =
  Printf.printf "prepare-component %s %s\n" component.name (Sys.getcwd ());
  ignore (with_component_dir ~strict:false component git_clean)

let prepare components =
  non_empty_iter prepare_component components

let forward_component component =
  ignore 
    (with_component_dir ~strict:false component
      (fun () ->
	git_push (Filename.concat (git_create_url component) component.name)))

let forward components =
  non_empty_iter forward_component components

let build_component_native component =
  if Sys.file_exists (with_rules ".bf-build" component) then
    log_message (component.name ^ " already built, nothing to do")
  else
    begin
      log_message (sprintf "building %s" (with_rules component.name component));
      Rules.build_rules component.rules;
      log_message (component.name ^ " built");
      let ch = open_out (with_rules ".bf-build" component) in
      output_string ch (string_of_float (Unix.gettimeofday ()));
      output_string ch "\n";
      close_out ch
    end

let build_component component =
  ignore 
    (with_component_dir ~strict:true component
      (fun () ->
	build_component_native component))
	  
let build components =
  non_empty_iter build_component components

let rebuild_component component =
  let file =
    sprintf "%s/%s" component.name
      (with_rules ".bf-build" component) in
  if Sys.file_exists file then
    Sys.remove file;
  ignore (with_component_dir ~strict:true component
    (fun () ->
      build_component_native component))
  
let rebuild components =
  non_empty_iter rebuild_component components

let rec scan_entry f dir =
  let dh = Unix.opendir dir in
  let rec read acc =
    try
      read ((Unix.readdir dh)::acc)
    with End_of_file ->
      Unix.closedir dh;
      acc      
  in 
  List.iter 
    (fun s ->
      if s <> "." && s <> ".." then
	let abs = Filename.concat dir s in
	let st = Unix.LargeFile.stat abs in
	let last =
	  match st.Unix.LargeFile.st_kind with
	    | Unix.S_LNK ->
		(Unix.LargeFile.lstat abs).Unix.LargeFile.st_mtime
	    | _ -> st.Unix.LargeFile.st_mtime
	in
	if System.is_directory abs then
	  (f (Dir abs,last); scan_entry f abs)
	else
	  f (File abs,last))
    (read [])

let create_top_state dir =
  match Params.get_param "autopkg" with
    | "true" -> 
	let t = Hashtbl.create 32 in
	scan_entry 
	  (fun (entry,last) ->
	    Hashtbl.add t entry last)
	  dir;
	t
    | _ -> (Hashtbl.create 0 : fs_state)
    
let strip_destdir s =
  let dest_dir = Params.get_param "dest-dir" in
  if dest_dir <> "" then
    let len = String.length dest_dir in
    if String.sub s 0 len = dest_dir then
      String.sub s len (String.length s - len)
    else s
  else s
   
let generate_changes rules top_dir a b =
  let string_of_fs_entry = function
    | File s -> "f " ^ (strip_destdir s)
    | Dir s  -> "d " ^ (strip_destdir s)
  in
  let entry_compare a b =
    match a, b with
      | File x, File y -> compare x y
      | Dir  x, Dir  y -> compare x y
      | File x, Dir  y -> compare x y
      | Dir  x, File y -> compare x y
  in
  match Params.get_param "autopkg" with
    | "true" ->
	let acc = ref [] in
	let out s = 
	  acc := s::!acc in
	Hashtbl.iter
	  (fun b_entry b_last ->
	    (try
	      let a_last = Hashtbl.find a b_entry in
	      if b_last > a_last then
		out b_entry
	    with Not_found ->
	      out b_entry))
	  b;
	let ch = open_out
	  (match rules with
	    | Some alt -> (".bf-list." ^ alt)
	    | None     ->  ".bf-list")
	in
	output_string ch
	  (sprintf "d %s\n" top_dir);
	List.iter
	  (fun e ->
	    output_string ch (string_of_fs_entry e);
	    output_string ch "\n")
	  (List.sort entry_compare !acc);
	close_out ch
    | _ -> ()

let install_component component =
  match component.label, component.pkg with
    | Tag _, Some pkg ->
	log_message
	  (component.name ^ (sprintf " must be installed by package (%s), noting to do" pkg));
	false
    | _ ->
	let result = ref false in
	ignore
	  (with_component_dir ~strict:false component
	    (fun () ->
	      if Sys.file_exists (with_rules ".bf-install" component)
		&& Sys.file_exists (with_rules ".bf-build" component) then
		  log_message ((with_rules component.name component) ^ " already installed, noting to do")
	      else
		begin
		  if not (Sys.file_exists (with_rules ".bf-build" component)) then
		    build_component_native component;
		  log_message ("installing " ^ component.name);
		  let top_dir =
		    Params.get_param "top-dir" in
		  let dest_dir =
		    Params.get_param "dest-dir" in
		  let real_dir =
		    if dest_dir = "" then
		      top_dir
		    else
		      Filename.concat dest_dir
			(System.strip_root top_dir)
		  in
		  let state =
		    create_top_state real_dir in
		  if dest_dir <> "" then
		    begin
		      Params.update_param "install-dir" real_dir;
		      Params.update_param "orig-top-dir" top_dir;
		      Params.update_param "top-dir" real_dir;
		    end;
		  Rules.install_rules component.rules;
		  Params.update_param "top-dir" top_dir;
		  generate_changes component.rules top_dir
		    state (create_top_state real_dir);
		  log_message (component.name ^ " installed");
		  let ch = open_out (with_rules ".bf-install" component) in
		  output_string ch (string_of_float (Unix.gettimeofday ()));
		  output_string ch "\n";
		  close_out ch;
		  result := true;
		end));
	!result
	  
let install components =
  List.fold_left
    (fun acc component ->
      let reinstalled =
	install_component component in
      if reinstalled then
	component::acc
      else
	acc)
    [] (non_empty_list components)

let reinstall_component component =
  let file =
    sprintf "%s/%s" component.name
      (with_rules ".bf-install" component) in
  if Sys.file_exists file then
    Sys.remove file;
  ignore(install_component component)

let reinstall components =
  non_empty_iter reinstall_component components

let update_component component =
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
	git_fetch "origin";
	git_fetch ~tags:true "origin";
	git_track_new_branches ();
	let update branch =
	  if git_changed branch (origin branch) then
	    begin
	      git_checkout ~force:true ~key:branch ();
	      git_clean ();
	      git_merge (origin branch);
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
  if component.name <> "pack" then
    local_changes || !remote_changes || status_changes
  else false

let update components =
  List.exists (fun x -> x) (List.map update_component components)

let fetch_tags component =
  ignore
    (with_component_dir ~strict:false component
      (fun () ->
	git_fetch "origin";
	git_fetch ~tags:true "origin";
	git_track_new_branches ()))    

(* Pack update support *)

exception Pack_current_branch_is_not_set

let pkgname_of_tag tag =
  try
    Some (String.sub tag 0 (String.index tag '/'))
  with Not_found -> None

let pkgname_of_specdir specdir =
  Filename.basename (Filename.dirname specdir)

let branch_of_specdir s =
  Filename.basename s

let reg_pkg_release specdir ver rev =
  let name = "release" in
  with_component_dir ~strict:true (make_component ~label:(Branch "master") "pack")
    (fun () ->
      let file = 
	sprintf "%s/%s/%s" (pkgname_of_specdir specdir) (branch_of_specdir specdir) name in
      System.append_write ~file (sprintf "%s %d\n" ver rev);
      Git.git_add file;
      Git.git_commit ~empty:true
	(sprintf "reg pkg release %s %s %s %d" 
	  (pkgname_of_specdir specdir) (branch_of_specdir specdir) ver rev);      
      Git.git_push_cycle ~tags:false ~refspec:None "origin" 5)
   
exception Pkg_release_not_found of (string * exn)

let rec last = function
  | [] -> raise Not_found
  | hd::[] -> hd
  | hd::tl -> last tl

let vr_compare a b =
  let r = compare (fst b) (fst a) in
  if r = 0 then
    compare (snd b) (snd a)
  else r

let max_vr l =
  List.hd (List.sort vr_compare l)
     
let read_pkg_release ?(next=false) ?version specdir =
  let with_next n = if next then succ n else n in
  let make s =
    let (ver,rev) =
      let pos = String.index s ' ' in
      String.sub s 0 pos,
      (with_next
	(int_of_string
	  (String.sub s (succ pos) (String.length s - pos - 1))))
    in (ver,rev)
  in
  let filter (v,r) =
    match version with
      | Some v' -> v' = v
      | None -> true
  in
  let file = Filename.concat specdir "release" in
  (try
    if Sys.file_exists file then
      let ch = open_in file in
      let vr =
	max_vr (List.filter filter
	  (List.map make (System.list_of_channel ch))) in
      close_in ch; vr
    else raise Exit
  with exn -> 
    raise (Pkg_release_not_found (specdir,exn)))

let mk_tag (pkgname,ver,rev) =
  sprintf "%s/%s-%d" pkgname ver rev

let tag_of_specdir specdir =
  try
    let pkgname = pkgname_of_specdir specdir in
    let (ver,rev) = read_pkg_release specdir in
    Some (mk_tag (pkgname, ver, rev))
  with exn -> log_message
    (sprintf "Warning: cannot parse release file from %s by error (%s)" specdir
      (Printexc.to_string exn));
    None

let update_pack component =
  let remote_changes = ref false in
  let local_changes =
    with_component_dir ~strict:true component
      (fun () ->
	let start = git_current_branch () in
	git_fetch "origin";
	git_fetch ~tags:true "origin";
	git_track_new_branches ();
	List.iter
	  (fun branch ->
	    if git_changed branch (origin branch) then
	      begin
		git_checkout ~force:true ~key:branch ();
		git_clean ();
		git_merge (origin branch);
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
       (install_component component); (* need for create .bf-build and .bf-install *)
  (local_changes || !remote_changes)

let pack_changes specdir component =
  let is_release s =
    let x = "release" in
    let sl = String.length s in
    let xl = String.length x in
    sl > xl && String.sub s (sl - xl) xl = x      
  in  
  let tag' = tag_of_specdir specdir in
  with_dir component.name
    (fun () ->
      match git_current_branch () with
	| Some cur ->
	    (match tag' with
		Some tag ->
		  let rex = Pcre.regexp 
		    (sprintf "%s/%s/"
		      (pkgname_of_specdir specdir)
		      (branch_of_specdir specdir))
		  in
		  (try
		    List.exists (fun s ->
		      let changed =
			(Pcre.pmatch ~rex s) && not (is_release s) in
		      if changed then
			log_message (sprintf "%s is changed" s);
		      changed)
		      (git_changes tag cur)
		  with Key_not_found key ->
		    log_message (sprintf "Warning: git-key (%s) is not found in pack" key);
		    true)
	      | None -> 
		  log_message (sprintf "Warning: cannot find current tag by specdir(%s)" specdir);
		  true)
	| None -> raise Pack_current_branch_is_not_set)
       
let status_component ?(max_component_length=0) ?(max_label_length=0) component =
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

let rec max_value f cur = function
  | [] -> cur
  | hd::tl ->
      max_value f (max (f hd) cur) tl

let status components =
  let max_component_length =
    max_value (fun n -> String.length n.name) 0 components in
  let max_label_length =
    max_value (fun n -> String.length (string_of_label n.label)) 0 components in
  non_empty_iter
    (fun component -> 
      status_component ~max_component_length ~max_label_length component)
    components

let tag_component tag component =
  let call () =
    let url = 
      Filename.concat
	(git_create_url component) component.name in
    match git_current_branch () with
	Some branch ->
	  (match git_make_tag tag with
	    | Tag_created ->
		git_push_cycle ~tags:true ~refspec:(Some tag) url 10
	    | Tag_already_exists -> 
		git_push_cycle ~tags:true ~refspec:(Some tag) url 10
	    | Tag_creation_problem -> raise Logger.Error)
      | None ->
	  log_message ("Warning: cannot find current branch for " ^ component.name);
	  (match git_make_tag tag with
	    | Tag_created ->
		git_push_cycle ~tags:true ~refspec:(Some tag) url 10
	    | Tag_already_exists -> 
		git_push_cycle ~tags:true ~refspec:(Some tag) url 10
	    | Tag_creation_problem -> raise Logger.Error)    
  in ignore 
       (with_component_dir ~strict:false component call)

let make_tag tag components =
  non_empty_iter (tag_component tag) components

let diff_component tag_a tag_b component =
  ignore (with_component_dir ~strict:false component
    (fun () ->
      print_endline (git_diff_view ~tag_a ~tag_b)))

let make_diff tag_a tag_b components =
  non_empty_iter (diff_component tag_a tag_b) components

let changelog_component ?(branch=None) ?(diff=false) ?(since=None) tag_a tag_b component =
  let chunks = ref [] in
  ignore (with_component_dir ~low:true ~strict:false component
    (fun () ->
      let pack =
	if component.name = "pack" then
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
	chunks := (Printf.sprintf "\n\n\n### %s (%s) (%s)\n\n"
	  (String.uppercase component.name)
	  (string_of_label_type component.label)
	  (string_of_label component.label))::logs));
  !chunks

let make_changelog ?(interactive=false) ?(compact=false) ?(branch=None) tag_a tag_b components =
  let chunks = ref [] in
  let add s = chunks:=s::!chunks in
  non_empty_iter
    (fun component -> 
      List.iter add (changelog_component ~branch ~diff:false tag_a tag_b component)) components;
  if not compact then
    begin
      add "\n------------------ DIFF -------------------\n";
      non_empty_iter
	(fun component ->
	  List.iter add (changelog_component ~branch ~diff:true tag_a tag_b component))
	components;
    end;
  if interactive then
    begin
      printf "bf@changelog %s -> %s\n" tag_a tag_b;
      List.iter print_endline (List.rev !chunks)
    end
  else
    Notify.send_message
      ~subject:(Printf.sprintf "bf@changelog %s -> %s" tag_a tag_b)
      ~contents:(List.rev !chunks)
      (Params.get_param "smtp-notify-email")

let make_review since components =
  ignore(update components);

  let chunks = ref [] in
  let add s = chunks:=s::!chunks in
  non_empty_iter
    (fun component -> 
      let tag = string_of_label component.label in
      List.iter add (changelog_component ~diff:false ~since:(Some since) tag tag component)) components;
  add "\n------------------ DIFF -------------------\n";
  non_empty_iter
    (fun component ->
      let tag = string_of_label component.label in
      List.iter add (changelog_component ~diff:true ~since:(Some since) tag tag component))
    components;

  Notify.send_message
    ~subject:(Printf.sprintf "bf@review (%s)" since)
    ~contents:(List.rev !chunks)
    (Params.get_param "smtp-notify-email")

let with_tag tag components =
  match tag with
    | None -> components
    | Some tag_name ->
	List.map
	  (fun component ->
	    {
	      name = component.name; 
	      label = Tag tag_name; 
	      pkg = component.pkg; 
	      rules = component.rules;
	    })
	  components

let only_local components =
  List.filter
    (fun c -> c.pkg = None) components

let only_external components =
  List.filter
    (fun c -> c.pkg <> None) components

let as_current l =
  List.map (fun c -> 
    { 
      name = c.name; 
      label = Current; 
      pkg = c.pkg; 
      rules = c.rules;
    }) l

let tag_ready ~tag components =
  List.for_all
    (fun component ->
      let res = ref false in
      ignore
	(with_component_dir ~strict:false component
	  (fun () -> res := List.mem tag (git_tag_list ())));
      !res)
    (as_current (only_local components))

let prepare_composite ?tag composite =
  log_message ("=> prepare-composite " ^ composite);
  prepare (with_tag tag (Rules.components_of_composite composite))

let update_composite ?tag composite =
  log_message ("=> update-composite " ^ composite);
  update (with_tag tag (Rules.components_of_composite composite))

let forward_composite ?tag composite =
  log_message ("=> forward-composite " ^ composite);
  forward (with_tag tag (Rules.components_of_composite composite))
  
let build_composite ?tag composite =
  log_message ("=> build-composite " ^ composite);
  build (with_tag tag (Rules.components_of_composite composite))

let rebuild_composite ?tag composite =
  log_message ("=> rebuild-composite " ^ composite);
  rebuild (with_tag tag (Rules.components_of_composite composite))

let install_composite ?tag composite =
  log_message ("=> install-composite " ^ composite);
  install (with_tag tag (Rules.components_of_composite composite))

let reinstall_composite ?tag composite =
  log_message ("=> reinstall-composite " ^ composite);
  reinstall (with_tag tag (Rules.components_of_composite composite))

let status_composite ?tag composite =
  log_message ("=> status-composite " ^ composite);
  status (with_tag tag (Rules.components_of_composite composite))

let tag_composite composite tag =
  log_message ("=> tag-composite " ^ composite ^ " " ^ tag);
  make_tag tag 
    (only_local
      (Rules.components_of_composite composite))

let review_composite composite since =
  log_message ("=> review-composite " ^ composite ^ " " ^ since);
  make_review since (Rules.components_of_composite composite)

let diff_composite composite tag_a tag_b =
  log_message ("=> diff-composite " ^ composite ^ " " ^ tag_a ^ ":" ^ tag_b);
  make_diff tag_a tag_b 
    (only_local (Rules.components_of_composite composite))

let changelog_composite ?(interactive=false) ?(compact=false) composite tag_a tag_b =
  log_message ("=> changelog-composite " ^ composite ^ " " ^ tag_a ^ ":" ^ tag_b);
  let branch =
    Some (Filename.basename (Filename.dirname composite)) in  
  make_changelog ~interactive ~compact ~branch tag_a tag_b
    (only_local (Rules.components_of_composite composite))

let changelog_components components tag_a tag_b =
  log_message ("=> changelog-components " ^ tag_a ^ ":" ^ tag_b);
  make_changelog tag_a tag_b (only_local components)

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
      ignore (install (Scheme.components_of_sval_array v))); Snull

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

let scm_copy_file src dst =
  System.copy_file
    (Scheme.string_of_sval src)
    (Scheme.string_of_sval dst);
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
  let contents = [Scheme.string_of_sval msg] in
  let recipients =
    List.map Scheme.string_of_sval
      (Scheme.list_of_sval rcpts) in
  List.iter
    (Notify.send_message ~subject ~contents)
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
  if Pcre.pmatch ~pat:(Scheme.string_of_sval templ) (Scheme.string_of_sval s)
  then Strue
  else Sfalse

let scm_substrings templ s =
  scm_make_list
    (function
      | None   -> Sstring ""
      | Some s -> Sstring s)
    (Array.to_list
      (Pcre.extract_opt
	~pat:(Scheme.string_of_sval templ)
	(Scheme.string_of_sval s)))

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

let scm_git_push_cycle url depth =
  Git.git_push_cycle
    ~tags:false
    ~refspec:None
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

Ocs_env.set_pfn Scheme.env scm_log_command "log-command";;
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
Ocs_env.set_pf2 Scheme.env scm_write_file "write-file";;
Ocs_env.set_pf2 Scheme.env scm_write_scheme_value "write-scheme-value";;
Ocs_env.set_pf2 Scheme.env scm_substring "substring?";;
Ocs_env.set_pf2 Scheme.env scm_substrings "substrings";;
Ocs_env.set_pfn Scheme.env scm_env_append "env-append";;

Ocs_env.set_pf2 Scheme.env scm_git_push_cycle "git-push-cycle";;

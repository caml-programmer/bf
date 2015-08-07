open Printf
open Types
open Logger
open Deptree

type forktype =
  | Trunk
  | Increment_branch
  | Extend_branch

let string_of_forktype = function
  | Trunk -> "trunk"
  | Increment_branch -> "increment-branch"
  | Extend_branch -> "extend-branch"

let write_release file l =
  let ch = open_out file in
  List.iter (fun s ->
    output_string ch s;
    output_string ch "\n") l;
  close_out ch

let change_release mode dst_capacity capacity_reduction depth local_depth specdir =
  let (ver,rev) =
    Release.get specdir in
  let ver = capacity_reduction ver in
  match mode with
    | Trunk ->
	if local_depth > depth then
	  (* для пакетов нижнего уровня *)
	  [sprintf "%s %d" (Version.major_increment dst_capacity ver) 0]
	else
	  (* для пакетов верхнего уровня *)
	  [sprintf "%s %d" (Version.minor_increment dst_capacity ver) 0]
    | Increment_branch ->
	[sprintf "%s %d" (Version.minor_increment dst_capacity ver) 0]
    | Extend_branch ->
	[sprintf "%s %d" (Version.extend ver) 0]

let make_external_depends packdir branch local_depends =
  List.filter
    (fun specdir ->
      let depfile = Filename.concat specdir "depends" in
      Sys.file_exists depfile && (not (List.mem_assoc specdir local_depends)))
    (List.map (fun s -> Filename.concat packdir (sprintf "%s/%s" s branch))
      (System.list_of_directory packdir))

let update_external_depends mode dst_capacity capacity_reduction local_depends specdir =
  let depends =
    Depends.parse (Filename.concat specdir "depends") in
  let fixed_depends =
    let change pkgname = function
      | None -> None
      | Some (op,ver) ->
	  let specdir' =
	    let d = Filename.dirname in
	    Filename.concat (d (d specdir))
	      (sprintf "%s/%s" pkgname (Specdir.branch specdir)) in
	  if List.mem_assoc specdir' local_depends then
	    let increment v =
	      match mode with
		| Trunk ->
		    Version.major_increment dst_capacity v
		| Increment_branch ->
		    Version.minor_increment dst_capacity v
		| Extend_branch ->
		    Version.extend v in
	    Some (op,increment (capacity_reduction ver))
	  else
	    Some (op,ver)
    in
    List.map (fun (os,deplist) ->
      os,(List.map (fun (pkgname,ov_opt,pkg_desc_opt) -> 
	(pkgname,(change pkgname ov_opt),pkg_desc_opt)) deplist))
      depends
  in
  Depends.write
    (Filename.concat specdir "depends") fixed_depends

exception Bad_project_branch_format of string
exception Branch_with_different_project of string
exception No_version_difference of string
exception Reject_downgrade_version of string
exception Reject_non_trunk_version of string
exception Destination_already_exists of string
exception Destination_branch_already_used of string

let parse_fork_branch specdir =
  let s = Specdir.branch specdir in
  try
    let len = String.length s in
    let pos = String.rindex s '-' in
    String.sub s 0 pos,
    String.sub s (succ pos) (len - pos - 1)
  with Not_found ->
    if Version.is s then
      ("skvt",s)
    else
      s, (fst (Release.get ~next:false specdir))
     
let fork_type specdir dst =
  let src = Specdir.branch specdir in
  if not (Version.exists dst) then
    raise (Bad_project_branch_format dst);

  if Version.exists src || Version.is src then
    let (src_project,src_version) =
      parse_fork_branch specdir in
    let (dst_project,dst_version) =
      parse_fork_branch (Filename.concat (Filename.dirname specdir) dst) in
    if src_project = dst_project then
      let src_ver = Version.parse src_version in
      let dst_ver = Version.parse dst_version in
      let src_len = List.length src_ver in
      let dst_len = List.length dst_ver in
      match Version.compare dst_ver src_ver with
	| 0  -> raise (No_version_difference dst)
	| -1 -> raise (Reject_downgrade_version dst)
	| 1 ->
	    if src_len = dst_len then
	      Increment_branch
	    else
	      Extend_branch
	| _ -> assert false
    else
      raise (Branch_with_different_project dst_project)
  else    
    let src_ver =
      Version.parse (snd (parse_fork_branch specdir)) in
    let dst =
      snd (parse_fork_branch (Filename.concat (Filename.dirname specdir) dst)) in
    let dst_ver =
      Version.parse dst in
    match Version.compare dst_ver src_ver with
      |  0 -> Trunk
      |  1 -> raise (Reject_non_trunk_version dst)
      | -1 -> raise (Reject_downgrade_version dst)
      |  _ -> assert false
      
let check_components =
  List.iter
    (fun c ->
      let component_location =
	let s = c.name in
	if Sys.file_exists s then s
	else s ^ ".git"
      in
      let checkout_state key =
	log_message (sprintf "prepare component state: %s (%s)" c.name key);
	
	if Sys.file_exists c.name then
	  ignore
	    (System.with_dir c.name
	      (fun () ->
		(match Git.git_current_branch () with
		  | Some key' ->
		      if key' <> key then
			Git.git_checkout ~force:true ~key ()
		  | _ -> Git.git_checkout ~force:true ~key ())))
	else
	  Component.prepare c
      in
      (match c.label with
	| Current ->
	    log_error (sprintf "used current branch for %s component forking\n%!" component_location)
	| Branch s | Tag s -> checkout_state s))

let commit_pack_changes (src,dst) pack_dir =
  System.with_dir pack_dir
    (fun () ->
      Git.git_add ".";
      Git.git_add "-u";
      Git.git_commit (sprintf "add new pack branch %s from %s" dst src);
      Git.git_push "origin")

let check_destination_branch packdir dst =
  List.iter (fun x ->
    let pkgdir = Filename.concat packdir x in
    let branch_path = Filename.concat pkgdir dst in
    if System.is_directory pkgdir && Sys.file_exists branch_path then
      raise (Destination_branch_already_used branch_path))
  (System.list_of_directory packdir) 

exception Not_found_specdir_in_deplist of string

let make ?(interactive=true) ?(depth=0) top_specdir dst =
  let dir = Filename.dirname in
  let packdir = dir (dir top_specdir) in

  Check.specdir top_specdir;
  Check.pack_component ();
  check_destination_branch packdir dst;

  let src = Specdir.branch top_specdir in
  let dst_specdir =
    dir top_specdir ^ "/" ^ dst in
  let dst_capacity =
    Version.capacity (snd (parse_fork_branch dst)) in

  if Sys.file_exists dst_specdir then
    raise (Destination_already_exists dst_specdir);

  let mode = fork_type top_specdir dst in

  let src_capacity =
    if mode = Trunk then
      Version.capacity (fst (Release.get ~next:false top_specdir))
    else
      Version.capacity (snd (parse_fork_branch src))
  in

  let capacity_reduction v =
    let c = Version.capacity v in
    let base =
      match mode with
	| Trunk ->
	    dst_capacity
	| Increment_branch ->
	    dst_capacity
	| Extend_branch ->
	    src_capacity
    in
    if c < base then
      let n =
	base - c in
      if n > 0 then
	v ^ "." ^  (Version.null_extend n)
      else v
    else v
  in

  log_message
    (sprintf "Create new pack branch %s from %s with mode [%s]:\n"
      dst src (string_of_forktype mode));

  let pack_dir = dir (dir top_specdir) in
  let deptree = Packtree.create ~default_branch:(Some src) top_specdir in
  let deplist = List.map (fun (k,v) -> fst k,v) (deplist_of_deptree deptree) in
  let depends = list_of_deptree deptree in
  log_message "depend list...";
  List.iter (fun s -> printf "%s\n" (fst s)) depends;

  let branch_jobs = ref [] in
  let regjob loc f =
    if not (List.mem_assoc loc !branch_jobs) then
      branch_jobs := (loc,f)::!branch_jobs in

  let change_components =
    List.map
      (fun c ->
	let component_location =
	  let s = c.name in
	  if Sys.file_exists s then s
	  else s ^ ".git"
	in
	let make_new_branch ?(start=None) () =
	  (* Git.git_pull "origin"; *)
	  if not (List.mem (Branch.origin dst) (Git.git_branch ~remote:true ())) then
	    if List.mem dst (Git.git_branch ()) then
	      Git.git_push ~refspec:dst "origin"
	    else
	      begin
		Git.git_create_branch ~start dst;
		Git.git_push ~refspec:dst "origin";
	      end
	  else
	    log_message (sprintf "warning: remote branch (%s/%s) already exists" component_location dst)
	in
	(match c.label with
	  | Current ->
	      printf "Warning: used current branch for %s component forking\n%!" c.name;
	      regjob component_location (make_new_branch ~start:None);
	      { c with label = Branch dst }
	  | Branch start ->
	      if c.name = "pack" then c
	      else
		begin
		  regjob component_location 
		    (make_new_branch ~start:(Some start));
		  { c with label = Branch dst }
		end
	  | Tag s ->
	      printf "Warning: tag %s unchanged while %s component forking\n%!" s c.name;
	      c))
  in
  
  let change_depends depends =
    let change pkgname ov_opt =
      let specdir =
	sprintf "%s/%s/%s" pack_dir pkgname src in
      let local_depth = 
	try
	  List.assoc specdir deplist 
	with Not_found ->
	  raise (Not_found_specdir_in_deplist specdir)
      in
      match ov_opt with
	| None -> None
	| Some (op,ver) ->
	    let increment v =
	      match mode with
		| Trunk ->
		    if local_depth > depth then
		      Version.major_increment dst_capacity v
		    else
		      Version.minor_increment dst_capacity v
		| Increment_branch ->
		    Version.minor_increment dst_capacity v
		| Extend_branch ->
		    Version.extend v
	    in
	    Some (op,increment (capacity_reduction ver))
    in
    List.map
      (fun (os,deplist) ->
	os,(List.map (fun (pkgname,ov_opt,pkg_desc_opt) ->
	  if Params.home_made_package pkgname then
	    (pkgname,(change pkgname ov_opt),pkg_desc_opt)
	  else
	    (pkgname,ov_opt,pkg_desc_opt)) deplist))
      depends
  in

  let check (specdir,_) =
    check_components
      (Composite.components
	(Filename.concat specdir "composite")) in

  let make_dst_specdir specdir =
    Filename.concat (dir specdir) dst in

  let fork_components (specdir,_) =
    let dst_specdir =
      make_dst_specdir specdir in
    let source =
      Filename.concat specdir in    
    let exists = Sys.file_exists in
    let destination =
      Filename.concat dst_specdir in

    let local_depth = List.assoc specdir deplist in
    let files = System.list_of_directory specdir in
    let components =
      Composite.components (source "composite") in

    Commands.make_directory [dst_specdir];
    
    let ignore = ["depends";"composite";"release"] in
    List.iter
      (fun n ->
	if not (List.mem n ignore) then
	  System.copy_file
	    (source n)
	    (destination n)) files;
    
    (* composite handling *)

    if exists (source "composite") && (not (exists (destination "composite"))) then
      Composite.write (destination "composite")
	(change_components components);
    
    (* depends handling *)
    
    if exists (source "depends") then
      begin
	match mode with
	  | Trunk ->
	      if exists (destination "depends") then
		begin (* it is necessary for aborting process after copy the depends file *)
		  let depends = Depends.parse (destination "depends") in
		  Depends.write (source "depends") (change_depends depends);
		end
	      else
		begin
		  let depends = Depends.parse (source "depends") in
		  System.copy_file (source "depends") (destination "depends");
		  Depends.write (source "depends") (change_depends depends);
		end
	  | Increment_branch | Extend_branch ->
	      let depends = Depends.parse (source "depends") in
	      Depends.write (destination "depends") (change_depends depends)
      end;

    (* release handling *)
    
    if exists (source "release") then
      begin
	match mode with
	  | Trunk ->
	      if exists (destination "release") then
		begin
		  write_release (source "release")
		    (change_release mode dst_capacity capacity_reduction depth local_depth dst_specdir);
		end
	      else
		begin
		  System.copy_file (source "release") (destination "release");
		  write_release (source "release")
		    (change_release mode dst_capacity capacity_reduction depth local_depth dst_specdir)
		end
	  | Increment_branch | Extend_branch ->
	      write_release (destination "release")
		(change_release mode dst_capacity capacity_reduction depth local_depth specdir)
      end
  in

  List.iter check depends;
  List.iter fork_components depends;

  if mode = Trunk then
    (* Для режимов отличных от trunk - обновление
       внешних зависимостей не производим *)
    List.iter (update_external_depends mode dst_capacity capacity_reduction depends)
      (make_external_depends pack_dir
	(Specdir.branch top_specdir)
	depends);

  List.iter (fun x ->
    log_message (sprintf "Need branching %s" (fst x))) !branch_jobs;

  if interactive then
    begin
      log_message "Delay before components branching...";
      Interactive.stop_delay 10;
    end;
  List.iter
    (fun (loc,f) -> 
      log_message (sprintf "Branching %s" loc);
      System.with_dir loc f)
    !branch_jobs;
  if interactive then
    begin
      log_message "Delay before commit pack changes...";
      Interactive.stop_delay 10;
    end;
  commit_pack_changes (src,dst) pack_dir

(* pack.ml *)

open Types
open Rules
open Commands
open Components
open Logger
open Ocs_env
open Ocs_types
open Platform
open Printf
open Spectype

open Deptree

type top_val = string * Types.version * Types.revision
type top_tree =
    top_val deptree

type pack_tree =
    (string * (Types.version * Types.revision option) option) deptree

let rec get_pack_depends ~default_branch table acc specdir =
  log_message (sprintf "resolve %s" specdir);
  let pkgdir =
    Filename.dirname (Filename.dirname specdir) in
  let f = Filename.concat specdir in
  let depends = Depends.load ~ignore_last:true (f "depends") in
  match
    (List.fold_left
      (fun acc (pkg,_,_) ->
	if Hashtbl.mem table pkg || not (Pcre.pmatch ~rex:(Pcre.regexp (sprintf "^%s" (Params.get_param "pkg-prefix"))) pkg) then
	  acc
	else
	  begin
	    Hashtbl.add table pkg false;
	    pkg::acc
	  end)
      [] depends)
  with
    | [] -> specdir::acc
    | l  ->
	(specdir::(List.flatten (List.map (fun pkg -> (get_pack_depends ~default_branch table [] (Specdir.of_pkg ~default_branch pkgdir pkg))) l)))

let deptree_of_pack ~default_branch specdir : pack_tree =
  let table = Hashtbl.create 32 in
  let pkgdir =
    Filename.dirname (Filename.dirname specdir) in
  let warning depth specdir =
    log_message (sprintf "%s warning: %s already scanned" (String.make depth ' ') specdir) in
  let resolve depth specdir =
    log_message (sprintf "%s resolve %s" (String.make depth ' ') specdir) in
  let ignore depth pkg =
    log_message (sprintf "%s ignore %s" (String.make depth ' ') pkg) in
  let rec make depth value =
    let specdir = fst value in
    if Hashtbl.mem table specdir then
      begin
	warning depth specdir;
	Dep_val (value, Dep_list [])
      end
    else
      if Sys.file_exists specdir then
	let depfile = Filename.concat specdir "depends" in
	Hashtbl.add table specdir false;
	if Sys.file_exists depfile then
	  let depends =
	    List.fold_left (fun acc (pkg,vr_opt,_) ->
	      try
		if Params.home_made_package pkg then
		  let new_specdir =
		    Specdir.of_pkg ~default_branch pkgdir pkg in
		  let new_value =
		    new_specdir, (Version.parse_vr_opt vr_opt) in
		  acc @ [new_value] (* add value/specdir for post-processing *)
		else
		  begin
		    ignore depth pkg;
		    acc
		  end
	      with exn ->
		log_message (sprintf "Warning: deptree_of_pack problem: %s\n" (Printexc.to_string exn));
		acc)
	      [] (Depends.load ~ignore_last:false depfile)
	  in
	  resolve depth specdir;
	  Dep_val (value, Dep_list
	    (List.fold_left
	      (fun acc value -> (try acc @ [make (succ depth) value] with Exit -> acc)) [] depends))
	else
	  begin
	    resolve depth specdir;
	    Dep_val (value, Dep_list [])
	  end
      else raise Exit
  in make 0 (specdir,None)

let toptree_of_specdir ?(log=true) specdir : top_tree =
  let table = Hashtbl.create 32 in
  let pkgdir =
    Filename.dirname (Filename.dirname specdir) in
  let warning depth specdir =
    if log then
      log_message (sprintf "%s warning: %s already scanned" (String.make depth ' ') specdir) in
  let resolve depth specdir =
    if log then
      log_message (sprintf "%s resolve %s" (String.make depth ' ') specdir) in
  let rec make depth specdir =
    
    if Hashtbl.mem table specdir then
      begin
	warning depth specdir;
	let (ver,rev) =
	  Hashtbl.find table specdir in
	Dep_val ((specdir,ver,rev), Dep_list [])
      end
    else
      if Sys.file_exists specdir then
	let depfile = Filename.concat specdir "depends" in
	
	let (ver,rev) =
	  try
	    Release.read specdir 
	  with 
	      Release.Not_found (pkg,exn) ->
		if log then
		  log_message (sprintf "%s fake-version %s by %s" (String.make depth ' ') specdir (Printexc.to_string exn));
		"0.0",0
	in
	Hashtbl.add table specdir (ver,rev);
	
	if Sys.file_exists depfile then
	  let depends =
	    List.fold_left (fun acc (pkg,_,_) ->
	      try
		let new_specdir = 
		  Specdir.of_pkg ~default_branch:(Some (Specdir.branch specdir)) pkgdir pkg in
		if Hashtbl.mem table new_specdir then
		  begin
		    acc @ [new_specdir] (* add specdir for post-processing *)
		  end
		else
		  acc @ [new_specdir]
	      with _ -> acc)
	      [] (Depends.load ~ignore_last:true depfile)
	  in
	  resolve depth specdir;
	  Dep_val ((specdir,ver,rev), Dep_list
	    (List.fold_left
	      (fun acc specdir -> (try acc @ [make (succ depth) specdir] with Exit -> acc)) [] depends))
	else
	  begin
	    resolve depth specdir;
	    Dep_val ((specdir,ver,rev), Dep_list [])
	  end
      else raise Exit
  in make 0 specdir

type dep_path = string list

exception Found_specdir of dep_path

let stop_delay n =
  printf "wait %d second>%!" n;
  for i = 1 to n do
    Unix.sleep 1;
    print_char ' ';
    print_int i;
    flush stdout;
  done;
  print_endline " go"

let upgrade specdir upgrade_mode default_branch =
  let specdir = System.path_strip_directory specdir in

  Check.specdir specdir;
  Check.pack_component ();

  let deptree =
    log_message "make depends tree...";
    deptree_of_pack ~default_branch specdir in
  
  let depends = list_of_deptree (map_deptree fst deptree) in
  log_message "depend list...";
  List.iter print_endline depends;
  
  stop_delay 5;

  let build_table = Hashtbl.create 32 in
  let mark_table = Hashtbl.create 32 in

  let find_specdir specdir =
    let rec make acc = function
      | Dep_val ((specdir',vr_opt), Dep_list l) ->
	  let new_acc = specdir'::acc in
	  if specdir = specdir' then
	    begin
	      match vr_opt with
		| Some (ver,rev_opt) ->
		    (match rev_opt with
		      | Some _ ->
			  acc :: (List.flatten (List.map (make new_acc) l))
		      | None -> [])
		| None -> []
	    end
	  else
	    List.flatten (List.map (make new_acc) l)
      | _ -> assert false
    in List.flatten (make [] deptree)
  in

  let eval_lazy_mode specdir =
    let dep_paths = 
      find_specdir specdir in
    if Hashtbl.mem mark_table specdir && not (Hashtbl.mem build_table specdir) then
      (false,dep_paths)
    else
      (true,dep_paths)
  in
  
  let complete_impl check_fs_packages =
    List.iter
      (fun specdir ->
	let (lazy_mode,dep_paths) =
	  eval_lazy_mode specdir in
	log_message (sprintf "lazy-mode is %b for %s, dep-paths:" lazy_mode specdir);
	List.iter log_message dep_paths;
	let updated =
	  Package.update
	    ~specdir
	    ~lazy_mode
	    ~check_pack:false
	    ~check_fs:check_fs_packages
	    ~interactive:true ()
	in
	Hashtbl.replace build_table specdir updated;
	if updated then
	  List.iter
	    (fun s -> Hashtbl.replace mark_table s true) dep_paths)
      depends
  in
  
  match upgrade_mode with
    | Upgrade_full ->
	List.iter
	  (fun specdir ->
	    ignore(Package.update ~specdir ~check_pack:false ~lazy_mode:false ~interactive:true ()))
	  depends
    | Upgrade_lazy ->
	List.iter
	  (fun specdir ->
	    ignore(Package.update ~specdir ~check_pack:false ~lazy_mode:true ~interactive:true ()))
	  depends
    | Upgrade_complete ->
	complete_impl false;
    | Upgrade_default ->
	complete_impl true

let top ?(replace_composite=None) ?depends specdir =
  let specdir =
    System.path_strip_directory specdir in
  Check.specdir specdir;
  Check.pack_component ();
  let depends =
    match depends with
      | Some deps -> deps
      | None ->
	  let deptree =
	    toptree_of_specdir specdir in
	  list_of_deptree deptree in
  log_message "depend list...";
  let components =
    List.fold_left
      (fun acc (specdir,_,_) ->
	log_message (sprintf "# %s" specdir);
	let composite =
      	  Filename.concat specdir "composite" in
	let new_components =
	  List.filter
	    (fun c -> not (List.mem c acc))
	    (Composite.components ~replace_composite composite) in
	List.iter 
	  (fun c ->
	    let rules =
	      match c.rules with None -> "" | Some s -> (" (rules " ^ s ^ ")") in
	    log_message 
	      (sprintf "\t- %s (%s %s)%s" c.name (string_of_label_type c.label) (string_of_label c.label) rules))
	  new_components;
	acc @ new_components)
      [] depends in
  stop_delay 3;
  let buf = Buffer.create 32 in
  let add = Buffer.add_string buf in
  List.iter
    (fun c ->
      let updated = 
	Component.update c in
      let reinstalled =
	Component.install ~snapshot:true c in
      add (sprintf "%s updated(%b) reinstalled(%b)\n" c.name updated reinstalled))
    components;
  print_endline (Buffer.contents buf)
   
let clone ?(vr=None) ~recursive ~overwrite specdir =
  let specdir = System.path_strip_directory specdir in

  Check.specdir specdir;
  Check.pack_component ();
  let deptree =
    if recursive then
      log_message "make depends tree...";
    Clone.deptree_of_specdir ~vr specdir in

  let depends =
    list_of_deptree deptree in
  List.iter (fun (s,_,_,_) -> printf "%s\n" s) depends;
  
  let with_rec l =
    (if recursive then l else [Lists.last l]) in

  log_message "depend list...";
  List.iter (fun (pkg,ver,rev,spec) -> printf "%s %s %d\n%!" pkg ver rev) (with_rec depends);
  stop_delay 5;
  
  let pkg_exists specdir ver rev =
    let rex =
      Pcre.regexp (sprintf "%s\\-%s\\-%d\\." (Specdir.pkgname specdir) ver rev) in
    List.exists (Pcre.pmatch ~rex) (System.list_of_directory ".")
  in
  
  List.iter
    (fun ((specdir,ver,rev,spec) as build_arg) ->
      if not (pkg_exists specdir ver rev) || overwrite then
	ignore(Package.build build_arg))
    (with_rec depends)


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
    Release.read specdir in
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
      s, (fst (Release.read ~next:false specdir))
     
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
  print_endline "check components...";
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

let fork ?(depth=0) top_specdir dst =
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
      Version.capacity (fst (Release.read ~next:false top_specdir))
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
  let deptree = deptree_of_pack ~default_branch:(Some src) top_specdir in
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
	  Git.git_pull "origin";
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
      let local_depth = List.assoc specdir deplist in
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

    make_directory [dst_specdir];
    
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
  log_message "Delay before components branching...";
  stop_delay 10;
  List.iter
    (fun (loc,f) -> 
      log_message (sprintf "Branching %s" loc);
      System.with_dir loc f)
    !branch_jobs;
  log_message "Delay before commit pack changes...";
  stop_delay 10;
  commit_pack_changes (src,dst) pack_dir

exception Cannot_create_image of string
exception Cannot_view_image of string

let graph ?ver ?rev specdir =
  let dotfile = "graph.dot" in
  let pngfile = "graph.png" in

  let vr =
    match ver,rev with
      | Some v, Some r -> Some (v,r)
      | _ -> None
  in

  let tree = Clone.deptree_of_specdir ~vr specdir in
  let depends =  
    list_of_deptree tree in
  
  List.iter 
    (fun (n,v,r,_) -> printf "%s %s %d\n" (Specdir.pkgname n) v r) depends;
  let ch = open_out dotfile in
  let out = output_string ch in
  
  out "digraph g {\n";
  out "graph [ rankdir = \"LR\" ];\n";
  out "node  [ fontname = \"Arial\", fontsize = \"10\" ];\n"; (* , //shape = record  *)
  out "edge  [ labelfontname = \"Arial\", labelfontsize = \"10\" ];\n";  (* , //style = dashed *)
  
  List.iter
    (fun (n,v,r,_) ->
      out (sprintf "\"%s\\n%s-%d\"
    [shape=box,style=\"rounded,filled\",fillcolor=\"#77CC77\"]\n" (Specdir.pkgname n) v r))
    depends;

  let rec write_links parent = function
    | Dep_val (e, tree) ->
	let (en,ev,er,_) = e in
	(match parent with
	  | Some p ->
	      let (pn,pv,pr,_) = p in
	      out (sprintf "\"%s\\n%s-%d\" -> \"%s\\n%s-%d\"\n" 
		(Specdir.pkgname pn) pv pr 
		(Specdir.pkgname en) ev er);
	      write_links (Some e) tree
	  | None ->
	      write_links (Some e) tree)
    | Dep_list l ->
	List.iter (write_links parent) l
  in

  let make_image () =
    if Sys.command (sprintf "dot -Tpng %s > %s" dotfile pngfile) <> 0 then
      raise (Cannot_create_image pngfile)
  in
  
  let view_image () =
    if Sys.command (sprintf "qiv -f %s" pngfile) <> 0 then
      if Sys.command (sprintf "gqview %s" pngfile) <> 0 then
	raise (Cannot_view_image pngfile)
  in

  write_links None tree;
  
  out "}\n";
  close_out ch;
  make_image ();
  view_image ()

let basegraph specdir mode =
  let dotfile = "graph.dot" in
  let pngfile = "graph.png" in

  let tree = deptree_of_pack ~default_branch:(Some (Specdir.branch specdir)) specdir in
  let depends =
    list_of_deptree (map_deptree fst tree) in

  let ch = open_out dotfile in
  let out = output_string ch in
  
  out "digraph g {\n";
  out "graph [ rankdir = \"LR\" ];\n";
  out "node  [ fontname = \"Arial\", fontsize = \"10\" ];\n"; (* , //shape = record  *)
  out "edge  [ labelfontname = \"Arial\", labelfontsize = \"10\" ];\n";  (* , //style = dashed *)
  
  let string_of_vr = function
    | Some (ver,rev_opt) ->
	sprintf "%s%s" ver (match rev_opt with Some rev -> sprintf "-%d" rev  | None -> "")
    | None -> "any"
  in
  
  List.iter
    (fun n ->
      out (sprintf "\"%s\" 
                    [shape=box,style=\"rounded,filled\",fillcolor=\"#77CC77\"]\n" (Specdir.pkgname n)))
    depends;

  let rec write_links parent = function
    | Dep_val (e, tree) ->
	let (en,evr_opt) = e in
	(match parent with
	  | Some p ->
	      let (pn,pvr_opt) = p in
	      if Version.have_revision evr_opt then
		if mode = "full" || mode = "hard" then
		  out (sprintf "\"%s\" -> \"%s\" [label=\"%s\", color=\"black\"]\n"
		    (Specdir.pkgname pn)
		    (Specdir.pkgname en)
		    (string_of_vr evr_opt))
		else ()
	      else
		if mode = "full" || mode = "soft" then
		  out (sprintf "\"%s\" -> \"%s\" [label=\"%s\", color=\"black\", style=\"dashed\"]\n"
		    (Specdir.pkgname pn)
		    (Specdir.pkgname en)
		    (string_of_vr evr_opt))
		else ();
	      write_links (Some e) tree
	  | None ->
	      write_links (Some e) tree)
    | Dep_list l ->
	List.iter (write_links parent) l
  in

  let make_image () =
    if Sys.command (sprintf "dot -Tpng %s > %s" dotfile pngfile) <> 0 then
      raise (Cannot_create_image pngfile)
  in
  
  let view_image () =
    if Sys.command (sprintf "qiv -f %s" pngfile) <> 0 then
      if Sys.command (sprintf "gqview %s" pngfile) <> 0 then
	raise (Cannot_view_image pngfile)
  in

  write_links None tree;
  
  out "}\n";
  close_out ch;
  make_image ();
  view_image ()


let vr_of_rev s =
  try
    let pos = String.index s '-' in
    let len = String.length s in
    String.sub s 0 pos,
    int_of_string (String.sub s (succ pos) (len - pos - 1))
  with _ -> raise (Invalid_argument s)

let diff_packages ?(changelog=false) specdir rev_a rev_b =
  let tree_a =
    Clone.deptree_of_specdir ~vr:(Some (vr_of_rev rev_a)) specdir in
  let tree_b =
    Clone.deptree_of_specdir ~vr:(Some (vr_of_rev rev_b)) specdir in
  Check.pack_component ();
  let depends_a =
    List.map (fun (p,v,r,s) -> p,(v,r))
      (list_of_deptree tree_a) in
  let depends_b =
    List.map (fun (p,v,r,s) -> p,(v,r))
      (list_of_deptree tree_b) in  
  List.iter
    (fun (pkgname_b,(ver_b,rev_b)) ->
      (try
	let (ver_a,rev_a) =
	  List.assoc pkgname_b depends_a in
	printf "# %s %s %d -> %s %d\n%!" pkgname_b ver_a rev_a ver_b rev_b;
	if changelog then
	  begin
	    let composite =
	      Filename.concat pkgname_b "composite" in
	    let pkgname = Specdir.pkgname pkgname_b in
	    let tag_a = sprintf "%s/%s-%d" pkgname ver_a rev_a in
	    let tag_b = sprintf "%s/%s-%d" pkgname ver_b rev_b in
	    if tag_a <> tag_b then
	      List.iter (List.iter print_endline)
		(List.map (Component.changelog tag_a tag_b)
		  (List.filter (fun c -> c.name <> "pack" && c.pkg = None && (not c.nopack)) (Composite.components composite)))
	  end
      with Not_found ->
	printf "+ %s %s %d\n%!" pkgname_b ver_b rev_b))
    depends_b;
      
  List.iter
    (fun (pkgname_a,(ver_a,rev_a)) ->
      if not (List.mem_assoc pkgname_a depends_b) then
	printf "- %s %s %d\n%!" pkgname_a ver_a rev_a)
    depends_a

let changelog_packages specdir rev_a rev_b =
  diff_packages ~changelog:true specdir rev_a rev_b


(* Versions *)

exception Bad_pkgdir of string
exception Bad_release of string

let last_versions pkgdir =
  let pkgname =
    Filename.basename pkgdir in
  let slash_re = Str.regexp_string "/" in
  let minus_re = Str.regexp_string "-" in
  
  let packdir = 
    Filename.dirname pkgdir in

  let read_pkg_version b = 
    let release =
      Filename.concat b "release" in
    if Sys.file_exists release then
      match Str.split (Str.regexp "\\ +") (Lists.last (System.list_of_channel (open_in release))) with
	| [ver;_] -> ver
	| _ ->
	    raise (Bad_release release)
    else b
  in

  if 
    Filename.basename packdir <> "pack" &&
    Filename.basename packdir <> "pack.git" 
  then
    raise (Bad_pkgdir pkgdir);

  System.with_dir packdir (fun () -> Git.git_pull "origin");
  
  System.with_dir pkgdir
    (fun () ->
      let branches =
	List.map
	  read_pkg_version (System.list_of_directory ".") in
      let t = ref ([] : ((string * string) * int) list) in
      let update (k,v) =
	if List.mem_assoc k !t then
	  let v' =  List.assoc k !t in
	  if v > v' then
	    t := List.map (fun (k',v') -> if k = k' then (k',v) else (k',v')) !t
	  else ()
	else	  
	  t := (k,v)::!t
      in
      let compare a b =
	compare (snd (fst a)) (snd (fst b)) in
      List.iter	update
	(List.fold_left
	  (fun acc tag ->
	    match Str.split slash_re tag with
	      | [pkg; fullver] ->
		  (try
		    let l =
		      Str.split minus_re fullver in
		    let ver = List.hd l in
		    let rev = int_of_string (List.hd (List.tl l)) in
		    if pkg = pkgname then
		      if List.mem ver branches then
			((pkg,ver),rev)::acc
		      else acc
		    else acc
		  with _ -> acc)
	      | _ -> acc)
	  [] (Git.git_tag_list ()));
      List.iter 
	(fun ((p,v),r) ->
	  printf "- %s/%s-%d\n%!" p v r) (List.sort compare !t))


(* Search *)

exception Bad_component of string
exception Bad_tag_file of string

let split del s =
  let pos = String.index s del in
  let len = String.length s in
  String.sub s 0 pos,
  String.sub s (succ pos) (len - pos - 1)

let read_tag_file file =
  match System.list_of_channel (open_in file) with
    | [x] -> x
    | _   -> raise (Bad_tag_file file)

let load_tags confdir =
  printf "load tags from %s\n%!" confdir;
  let start =
    Filename.concat confdir "refs/tags" in
  let rec fs_load f tag path =
    if System.is_directory path then
      List.iter
	(fun s ->
	  let new_tag =
	    if tag = "" then s else tag ^ "/" ^ s in
	  let new_path =
	    path ^ "/" ^ s in
	  fs_load f new_tag new_path)
	(System.list_of_directory path)
    else
      f tag (read_tag_file path)
  in
  let packed_load f =
    let re = Str.regexp_string "refs/tags/" in
    let file =
      Filename.concat confdir "packed-refs" in
    if Sys.file_exists file then
      List.iter
	(fun s -> 
	  let l = String.length s in
	  if l > 0 && s.[0] <> '#' && s.[0] <> '^' then
	    begin
	      let (id,r) = split ' ' s in
	      let rl = String.length r in
	      if Str.string_match re r 0 then
		begin
		  let tag =
		    String.sub r 10 (rl - 10) in
		  f tag id
		end
	    end)
	(System.list_of_channel (open_in file))
  in
  let t = Hashtbl.create 32 in
  let f tag id =
    Hashtbl.add t tag id in

  fs_load f "" start;
  packed_load f;
  t

exception Bad_tag of string

let split_tag s =
  let (n,v) = split '/' s in
  let (v,r) = split '-' v in
  try
    (n,(v,int_of_string r))
  with _ -> raise (Bad_tag s)

let replace_value k v =
  List.map
    (fun x' -> if (fst x') = k then k,v else x')

let only_minimal_revisions tags =
  let l = ref [] in
  List.iter
    (fun (n,(v,r)) ->
      try
	let cr = List.assoc (n,v) !l in
	if r < cr then
	  l := replace_value (n,v) r !l
	else ()
      with Not_found ->
	l := ((n,v),r)::!l)
    (List.map split_tag tags);
  List.sort 
    (fun a b -> compare (fst a) (fst b)) !l  

let load_commit_tags branch commit_id =
  let make_tag_list s =
    let tag_re = Str.regexp "tag: \\([^,)]+\\)[,)]" in
    let len = String.length s in
    let rec make acc start =
      if start >= len then
	acc
      else
	(try
	  ignore(Str.search_forward tag_re s start);
	  make
	    ((Str.matched_group 1 s)::acc) (Str.match_end ())
	with Not_found -> acc)
    in make [] 0
  in
  let cmd =
    sprintf "git log --oneline --decorate --abbrev=40 %s" branch in
  let direct = ref [] in
  let reverse = ref [] in
  let add (id,tags) =
    direct := (id,tags)::!direct;
    List.iter 
      (fun tag ->
	reverse := (tag,id)::!reverse) tags
  in
  let ch = Unix.open_process_in cmd in
  try
    while true do
      let (id,rest) = split ' ' (input_line ch) in
      let tags' =
	make_tag_list rest in
      if id = commit_id then
	begin
	  add (id,tags');
	  raise Exit
	end
      else
	add (id,tags')
    done; ([],[])
  with 
    | Exit -> close_in ch; (!direct,!reverse)
    | End_of_file ->
	close_in ch; ([],[])

exception Not_found_pack
exception Bad_pack_state of string

let search_pack () =
  let parent = Filename.dirname (Sys.getcwd ()) in
  let f = Filename.concat parent in
  if Sys.file_exists (f "pack.git") then
    f "pack.git"
  else if Sys.file_exists (f "pack") then
    f "pack"
  else raise Not_found_pack

let search commit_id =
  let dir = Sys.getcwd () in
  let component = Filename.basename dir in
  let confdir = ".git" in
  let packdir = search_pack () in
  
  (* Check pack state *)
  System.with_dir packdir (fun () ->
    printf "check pack state...";
    match Git.git_current_branch () with
      | Some "master" -> printf "ok\n%!"
      | _ -> raise (Bad_pack_state packdir));  

  let tag_list = load_tags confdir in
  let pack_tag_list =
    load_tags (Filename.concat packdir ".git") in
  let search_top_packages branch =
    (* Top-ы это те пакеты, которые не встречаются ни у
       какого-либо другого пакета в зависимостях *)
    let plist =
      List.fold_left
	(fun acc pkg ->
	  let specdir = Filename.concat pkg branch in
	  if Sys.file_exists specdir then
	    let depends =
	      Depends.parse (Filename.concat specdir "depends") in
	    (pkg,depends)::acc
	  else acc) []
	(List.filter System.is_directory (System.list_of_directory "."))
    in
    let mem pkg depends =
      List.exists (fun (os,deplist) -> List.exists (fun (pkg',_,_) -> pkg = pkg') deplist) depends in
    List.fold_left
      (fun acc (pkg,_) ->
	if List.exists (fun (_,depends) -> mem pkg depends) plist then
	  acc
	else pkg::acc) [] plist
  in
  let search_top_tags search_eq_tags component_branch tag =
    let packdir = search_pack () in
    let (component_package,_) = split_tag tag in
    let equivalents = search_eq_tags tag in
    let tops = ref [] in
    System.with_dir packdir
      (fun () ->
	(* Состояние pack-а необходимо перевести на момент создания тега *)
	Git.git_checkout ~low:true ~key:tag ();
	try
	  List.iter 
	    (fun pkg_branch ->	  
	      let pkg_list =
		search_top_packages pkg_branch in
	      let tree_list =
		List.fold_left
		  (fun acc pkg -> 
		    let specdir = Filename.concat pkg pkg_branch in
		    if Sys.file_exists specdir then
		      (pkg,(toptree_of_specdir ~log:false specdir))::acc
		    else acc) [] pkg_list in
	      let top_pkg_list_with_component =
		List.fold_left 
		  (fun acc (pkg,tree) ->
		    (* printf "top-pkg %s\n" pkg; *)
		    if
		      List.exists
			(fun (pkg',ver',rev') ->
			  let composite = sprintf "%s/composite" pkg' in (* here pkg with branch *)
			  (* printf "check %s\n" composite; *)
			  if Sys.file_exists composite then
			    List.exists
			      (fun c ->
				(c.name = component || c.name ^ ".git" = component) && c.label = Branch component_branch)
			      (Composite.components composite)
			  else false)
			(list_of_deptree tree)
		    then
		      pkg::acc
		    else acc) [] tree_list in
	      List.iter
		(fun top_pkg ->
		  let specdir = Filename.concat top_pkg pkg_branch in
		  let mktag (v,r) =
		    sprintf "%s/%s-%d" top_pkg v r in
		  if Sys.file_exists specdir then
		    begin
		      (*printf "checkout pack to state: %s for read %s/release\n%!" tag specdir;*)
		      Git.git_checkout ~low:true ~key:tag ();
		      let (v,r) = Release.read ~next:false specdir in
		      (*printf "read release %s-%d from specdir (%s)\n%!" v r specdir;*)
		      let rec find (v',r') =
			let key = mktag (v',r') in
			let check_rev () =
			  if r' > r + 2 then
			    raise Not_found
			  else
			    find (v',succ r')
			in
			if Hashtbl.mem pack_tag_list key then
			  (try
			    let tree =
			      Clone.deptree_of_specdir ~log:false ~vr:(Some (v',r')) ~packdir specdir in
			    if
			      List.exists 
				(fun (p',v',r',_) ->
				  let p'' =
				    Filename.basename (Filename.dirname p') in
				  List.exists 
				    (fun e ->
				      let (pe,(ve,re)) = split_tag e in
				      p'' = pe && v' = ve &&  r' = re)
				    equivalents)
				(list_of_deptree tree)
			    then key
			    else
			      check_rev ()
			  with 
			    | Logger.Error -> check_rev ()
			    | Tree_error msg ->
				print_string " ## ";
				print_string msg;
				check_rev ())
			else
			  begin
			    (* printf "tag not found: %s\n%!" key; *)
			    check_rev ()
			  end
		      in 
		      try
			tops := (find (v,r))::!tops
		      with Not_found ->
			tops := (top_pkg ^ " not found by 3 up-levels"):: !tops
		    end)
		top_pkg_list_with_component)
	    (System.list_of_directory component_package);
	  
	  (* Восстанавливаем состояние pack-а *)
	  Git.git_checkout ~low:true ~key:"master" ()
	with exn ->
	  Git.git_checkout ~low:true ~key:"master" ();
	  raise exn);
    !tops
  in

  List.iter
    (fun component_branch ->
      printf "Search by component branch: %s\n%!" component_branch;
      let (direct,reverse) =
	load_commit_tags component_branch commit_id in
      let tags =
	List.map
	  (fun ((n,v),r) -> sprintf "%s/%s-%d" n v r)
	  (only_minimal_revisions
	    (List.flatten (List.map snd direct))) in
      let search_equivalent_tags tag =
	try
	  let id =
	    List.assoc tag reverse in
	  List.assoc id direct
	with Not_found -> [tag]
      in
      List.iter
	(fun tag ->
	  let tops = 
	    (try
	      search_top_tags search_equivalent_tags component_branch tag
	    with Not_found -> [])
	  in
	  printf "> %s %s:\n%!" (Hashtbl.find tag_list tag) tag;
	  List.iter (printf " >> %s\n%!") tops) tags)
    (Git.git_branch ())

(* Clean old packages *)

let pkg_compare a b =
  let (dir_a,name_a,pkg_a,platform_a,ext_a,arch_a,ver_a,rev_a) = snd a in
  let (dir_b,name_b,pkg_b,platform_b,ext_b,arch_b,ver_b,rev_b) = snd b in
  match compare name_a name_b with
    | 0 ->
	(match Version.compare (Version.parse ver_a) (Version.parse ver_b) with
	  | 0 -> compare rev_a rev_b
	  | x -> x)
    | x -> x
  
let clean () =  
  let t = Hashtbl.create 32 in  
  List.iter
    (fun (s,x) ->
      let (_,name,_,_,_,_,ver,rev) = x in
      if Hashtbl.mem t name then
	Hashtbl.replace t name ((s,ver,rev)::(Hashtbl.find t name))
      else      
	Hashtbl.add t name [s,ver,rev])
    (List.sort pkg_compare
      (List.fold_left
	(fun acc s ->
	  try
	    (s,Pkgpath.parse s)::acc
	  with _ -> acc)
	[] (System.list_of_directory ".")));
  let droplist = ref [] in
  let drop s =
    droplist := s::!droplist in 
  Hashtbl.iter
    (fun name list ->
      let first = ref true in
      printf "%s: %s\n" name 
	(String.concat " " 
	  (List.map
	    (fun (s,ver,rev) ->
	      let label = 
		ver ^ "-" ^ string_of_int rev in
	      if !first then
		begin first := false; "[" ^ label ^ "]" end
	      else 
		begin drop s; label end) list))) t;
  try
    while true do
      printf "Clean unselected revisions? (y/n): %!";
      match input_line stdin with
	| "y" ->
	    List.iter Unix.unlink !droplist;
	    raise Exit
	| "n" ->
	    raise Exit
	| _ -> ()
    done
  with Exit -> ()

let droptags lifetime =
  let period =
    Lifetime.parse lifetime in
  Lifetime.iter period Git.resolve_tag_date
    (fun tag ->
      printf "drop %s\n%!" tag;
      Git.git_drop_tag tag)
    (Git.git_tag_list ())


(* Snapshot *)

let make_snapshot_id () =
  let t = Unix.localtime (Unix.time ()) in
  let ver = 
    sprintf "SS%04d%02d%02d"
      (t.Unix.tm_year+1900)
      (t.Unix.tm_mon+1)
      t.Unix.tm_mday in
  let rev =
    sprintf "%02d%02d%02d"
      (t.Unix.tm_hour)
      (t.Unix.tm_min)
      t.Unix.tm_sec in
  (ver, rev)

let snapshot ?(composite=None) specdir =
  let specdir =
    System.path_strip_directory specdir in
  let (ver,rev) =
    make_snapshot_id () in
  Check.specdir specdir;
  Check.pack_component ();
  let toptree =
    log_message "make depends tree...";
    toptree_of_specdir specdir in
  let depends = list_of_deptree toptree in
  log_message "depend list...";
  List.iter (fun (specdir,_,_) -> print_endline specdir) depends;
  stop_delay 5;
  top ~replace_composite:composite ~depends specdir;
  List.iter
    (fun (specdir,_,_) ->
      Pkgbuild.build_package_file ~snapshot:true (specdir,ver,rev))
    depends

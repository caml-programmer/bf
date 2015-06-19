(* Clone support *)

open Deptree
open Platform
open Spectype
open Logger
open Printf

type clone_val = string * Types.version * Types.revision * spec

type pkg_clone_tree =
    Pkgpath.t deptree

type clone_tree =
    clone_val deptree

open Pkgpath
     
let new_only e =
  with_platform
    (fun os platform ->
      let pkg =
	sprintf "%s-%s-%d.%s.%s.%s"
	  e.pkg_name e.pkg_version e.pkg_revision
	  (string_of_platform platform)
	  (System.arch ())
	  e.pkg_extension
      in
      if Sys.file_exists pkg then
	begin
	  log_message (sprintf "\tpackage %s/%s already exists" (Sys.getcwd ()) pkg);
	  false
	end
      else true)

let with_overwrite ow l =
  if ow then l else List.filter new_only l

let optint_of_string s =
  try
    Some (int_of_string s)
  with Failure(int_of_string) ->
    None

let compare_pkg_versions ver1 ver2 =
  let rex = Str.regexp ".-" in
  Version.compare 
    ~retype:optint_of_string
    (Str.split rex ver1)
    (Str.split rex ver2)

let soft_dep pkg_name pkg_path ver =
  let rex =
    Pcre.regexp (sprintf "%s-%s" pkg_name ver) in
  let dist_path =
    Filename.dirname pkg_path in
  let files =
    List.filter
      (Pcre.pmatch ~rex)
      (System.list_of_directory dist_path) in
  let dep_package = 
    List.fold_left
      (fun acc file ->
	match acc with
	  | None -> Some file
	  | Some acc_file ->
	      if compare_pkg_versions acc_file file > 0 then
		acc
	      else
		Some file) None files in
  match dep_package with
    | None   -> raise (Cannot_resolve_soft_dependes pkg_name)
    | Some p ->
	sprintf "%s/%s" dist_path p

let tree_of_package ?userhost pkg_path : pkg_clone_tree =
  let pre_table = Hashtbl.create 32 in
  
  let rec scan pkg_path =
    log_message (sprintf "scanning %s" pkg_path);
    let e = make_pkg_record ~userhost pkg_path in
    let deps = Pkgdeps.extract ~userhost pkg_path in
    Hashtbl.add pre_table e.pkg_name (e,deps);

    List.iter 
      (fun (pkg_name,ver_opt,rev_opt,operand_opt) ->
        (match ver_opt, rev_opt with
	  | Some ver, Some rev ->
	      if Hashtbl.mem pre_table pkg_name then
		begin
		  let (e,_) = Hashtbl.find pre_table pkg_name in
		  if ver <> e.pkg_version || rev <> e.pkg_revision then
		    begin
		      log_message (sprintf "Already registered: pkg(%s) ver(%s)/rev(%d) and next found: ver(%s)/rev(%d) not equivalent."
		      pkg_name e.pkg_version e.pkg_revision ver rev);
		      raise (Cannot_resolve_dependes pkg_path)
		    end
		end;
	      let new_path =
		sprintf "%s/%s-%s-%d.%s.%s.%s" e.pkg_dir pkg_name ver rev (string_of_platform e.pkg_platform) e.pkg_arch e.pkg_extension in
	      if not (Hashtbl.mem pre_table pkg_name) then
		scan new_path
	  | _ ->
	      ()))
    deps
  in
  scan pkg_path;

  let table = Hashtbl.create 32 in
  let warning depth s =
    log_message (sprintf "%s warning: %s already scanned" (String.make depth ' ') s) in
  let resolve depth s =
    log_message (sprintf "%s resolve %s" (String.make depth ' ') s) in
  let rec make depth pkg_path =
    if Hashtbl.mem table pkg_path then
      begin
	warning depth pkg_path;
	Dep_val (fst (Hashtbl.find pre_table (Pkgpath.name pkg_path)), Dep_list [])
      end
    else
      let pkg_name = Pkgpath.name pkg_path in
      let (e,deps) = Hashtbl.find pre_table pkg_name in
      Hashtbl.add table pkg_path (e.pkg_version,e.pkg_revision);

      let depend_paths =
	List.fold_left
	  (fun acc (pkg_name,ver_opt,rev_opt,operand_opt) ->
	    try
	      let extract_version ver_opt = 
		match ver_opt with
		  | Some v -> v
		  | None ->
		      try
			let (e,_) =
			  Hashtbl.find pre_table pkg_name in
			e.pkg_version
		      with Not_found ->
			log_error (sprintf "cannot resolve version for %s" pkg_name)
	      in
	      let ver = extract_version ver_opt in
	      let rev =
		match rev_opt with
		  | Some r -> r
		  | None ->
		      let (e,_) =
			try
			  Hashtbl.find pre_table pkg_name
			with Not_found ->
			  log_message (sprintf "warning: cannot resolve revision for %s" pkg_name);
			  raise Not_found in
		      e.pkg_revision in
	      (sprintf "%s/%s-%s-%d.%s.%s.%s" e.pkg_dir pkg_name ver rev
		(string_of_platform e.pkg_platform) e.pkg_arch e.pkg_extension)::acc
	    with Not_found -> acc) [] deps
      in
      resolve depth pkg_path;
      Dep_val (e, Dep_list
	(List.fold_left
	  (fun acc path -> (try acc @ [make (succ depth) path] with Exit -> acc)) [] depend_paths))
  in 

  make 0 pkg_path
       
let rec print_depends depth = function
  | Dep_list l ->
      List.iter (fun v -> print_depends (succ depth) v) l
  | Dep_val (e, tree) ->
      let step = String.make depth ' ' in
      printf "%s%s %s %s %d\n" step e.pkg_name e.pkg_branch e.pkg_version e.pkg_revision;
      print_depends (succ depth) tree

let print_dep_val e =
  printf "%s-%s-%d.%s.%s %s\n"
    e.pkg_name e.pkg_version e.pkg_revision e.pkg_arch e.pkg_extension e.pkg_branch
    
let clone_packages l =
  List.iter (fun e ->
    let specdir =
      sprintf "./pack/%s/%s" e.pkg_name e.pkg_branch in
    ignore (Package.update ~specdir ~ver:(Some e.pkg_version) ~rev:(Some (string_of_int e.pkg_revision)) ())) l

let rec download_packages userhost l =
  List.iter 
    (fun e ->
      let src =
	sprintf "%s:%s" userhost e.pkg_path in
      Commands.send_file_over_ssh src ".") l

let by_pkgfile userhost pkg_path mode =
  Params.update_param "clone-mode" "true";

  let overwrite = mode <> "default" in
  let depends =
    tree_of_package ~userhost pkg_path in

  print_endline "Depends Tree:";
  print_depends 0 depends;
  match mode with
    | "overwrite" -> clone_packages (list_of_deptree depends)
    | "depends"   ->
	print_endline "After Resort Order:";
	List.iter print_dep_val (list_of_deptree depends)
    | "packages"  -> download_packages userhost (list_of_deptree depends)
    | _           -> clone_packages (with_overwrite overwrite (list_of_deptree depends))

let tree_of_specdir ?(log=true) ?packdir ~vr specdir : clone_tree =
  let table = Hashtbl.create 32 in
  let pkgdir =
    match packdir with
      | Some p -> p
      | None ->
	  Filename.dirname (Filename.dirname specdir) in
  let warning depth specdir ver rev iver irev =
    if log then
      log_message (sprintf "%s warning: %s %s %d already scanned, ignore %s %d" (String.make depth ' ') specdir ver rev iver irev) in
  let replace depth specdir ver rev iver irev =
    if log then
      log_message (sprintf "%s warning: %s %s %d already scanned, replaced %s %d" (String.make depth ' ') specdir ver rev iver irev) in
  let resolve depth specdir ver rev =
    if log then
      log_message (sprintf "%s resolve %s %s %d" (String.make depth ' ') specdir ver rev) in
  let checkout_pack key =
    System.with_dir pkgdir
      (Git.git_checkout ~low:true ~key) in

  let (ver,rev) =
    match vr with
	Some x -> x | None -> Release.get specdir in
  
  let rec make depth (specdir,ver,rev,mode) =
    if Hashtbl.mem table specdir then
      begin
	let (ver',rev',_) =
	  Hashtbl.find table specdir in	
	if mode then
	  begin	    
	    checkout_pack 
	      (Tag.mk ((Specdir.pkgname specdir), ver, rev));
	    let spec =
	      Specload.v2
		~version:ver
		~revision:(string_of_int rev) specdir in
	    Hashtbl.replace table specdir (ver,rev,spec);
	    replace depth specdir ver' rev' ver rev;
	  end
	else
	  warning depth specdir ver' rev' ver rev;
	Dep_val (specdir, Dep_list [])
      end
    else
      begin
	checkout_pack 
	  (Tag.mk ((Specdir.pkgname specdir), ver, rev));
	
	if Sys.file_exists specdir then
	  let spec = 
	    Specload.v2
	      ~version:ver
	      ~revision:(string_of_int rev) specdir in
	  
	  Hashtbl.add table specdir (ver,rev,spec);
	  
	  let depfile = 
	    Filename.concat specdir "depends" in
	  if Sys.file_exists depfile then
	    let depends =
	      List.fold_left (fun acc (pkg,vr_opt,_) ->
		try
		  if Params.home_made_package pkg then
		    begin
		      let new_specdir =
			Specdir.of_pkg ~default_branch:(Some (Specdir.branch specdir)) pkgdir pkg in
		      let (ver,rev) = 
			Release.get new_specdir in
		      acc @ [new_specdir,ver,rev,(Version.have_revision (Version.parse_vr_opt vr_opt))] (* add specdir for post-processing *)
		    end
		  else acc
		with _ -> acc)
		[] (Depends.load ~ignore_last:false depfile)
	    in
	    resolve depth specdir ver rev;
	    Dep_val (specdir, Dep_list
	      (List.fold_left
		(fun acc (specdir,ver,rev,mode) ->
		  (try acc @ [make (succ depth) (specdir,ver,rev,mode)] with Exit -> acc)) [] depends))
	  else
	    begin
	      resolve depth specdir ver rev;
	      Dep_val (specdir, Dep_list [])
	    end
	else raise Exit
      end
  in
  
  let tree =
    try
      make 0 (specdir,ver,rev,true)
    with Exit -> checkout_pack "master";
      raise (Tree_error (sprintf "not found specdir (%s) for pack state: %s/%s-%d\n%!" specdir (Specdir.pkgname specdir) ver rev));
  in

  checkout_pack "master";
  
  (map_deptree (fun specdir -> 
    let (ver,rev,spec) =
      try
	Hashtbl.find table specdir 
      with Not_found -> assert false
    in (specdir,ver,rev,spec)) 
    tree)

let make ?(vr=None) ~recursive ~overwrite specdir =
  Params.update_param "clone-mode" "true";

  let specdir = System.path_strip_directory specdir in

  Check.specdir specdir;
  Check.pack_component ();
  let deptree =
    if recursive then
      log_message "make depends tree...";
    tree_of_specdir ~vr specdir in

  let depends =
    list_of_deptree deptree in
  List.iter (fun (s,_,_,_) -> printf "%s\n" s) depends;
  
  let with_rec l =
    (if recursive then l else [Lists.last l]) in

  log_message "depend list...";
  List.iter (fun (pkg,ver,rev,spec) -> printf "%s %s %d\n%!" pkg ver rev) (with_rec depends);
  Interactive.stop_delay 5;
  
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

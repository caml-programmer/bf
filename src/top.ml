open Printf
open Deptree
open Component
open Logger

type top_val = string * Component.version * Component.revision
type top_tree = top_val deptree

let tree_of_specdir ?(log=true) specdir : top_tree =
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
	    Release.get specdir 
	  with 
	      Release.Release_not_found error ->
		if log then
		  log_message (sprintf "%s fake-version %s by %s"
		    (String.make depth ' ') specdir (sprintf "release not found by %s" error));
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
	      [] (Spectype.depload ~ignore_last:true depfile)
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

let make ?(replace_composite=None) ?depends specdir =
  let specdir =
    System.path_strip_directory specdir in
  Check.specdir specdir;
  Check.pack_component ();
  let depends =
    match depends with
      | Some deps -> deps
      | None ->
	  let deptree =
	    tree_of_specdir specdir in
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
  Interactive.stop_delay 3;
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

open Logger
open Printf
open Deptree

type pack_tree =
    (string * (Component.version * Component.revision option) option) deptree

exception No_specdir of string

let create ?(all_platforms=false) ?(log=true) ~default_branch specdir : pack_tree =
  let table = Hashtbl.create 32 in
  let pkgdir =
    Filename.dirname (Filename.dirname specdir) in
  let warning depth specdir =
    if log then
      log_message (sprintf "%s warning: %s already scanned" (String.make depth ' ') specdir) in
  let resolve depth specdir =
    if log then
      log_message (sprintf "%s resolve %s" (String.make depth ' ') specdir) in
  let ignore depth pkg =
    if log then
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
		if log then
		  log_message (sprintf "Warning: deptree_of_pack problem: %s\n" (Printexc.to_string exn));
		acc)
	      [] (Spectype.depload ~all_platforms ~ignore_last:false depfile)
	  in
	  resolve depth specdir;
	  Dep_val (value, Dep_list
	    (List.fold_left
	      (fun acc value -> (try acc @ [make (succ depth) value] with No_specdir _ -> acc)) [] depends))
	else
	  begin
	    resolve depth specdir;
	    Dep_val (value, Dep_list [])
	  end
      else raise (No_specdir specdir)
  in make 0 (specdir,None)


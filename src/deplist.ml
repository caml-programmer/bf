open Printf
open Logger
open Deptree
open Pkgpath
open Clonetree

let table = Hashtbl.create 32

let rejects_file =
  ".bf-rejects"

let ignore_list =
  if Sys.file_exists rejects_file then
    System.list_of_file rejects_file
  else
    [(*"rpmlib";"/bin/bash";"/bin/sh"*)]

let valid_dep dep =
  List.for_all 
    (fun x ->
      not (Strings.substring_exists x dep)) ignore_list

let print_dep_val (e,extdeps) =
  printf "%s-%s-%d.%s.%s %s\n%!"
    e.pkg_name e.pkg_version e.pkg_revision e.pkg_arch e.pkg_extension
    e.pkg_branch;    
  List.iter
    (fun dep ->
      if valid_dep dep && not (Hashtbl.mem table dep) then
	begin
	  Hashtbl.add table dep true;
	  printf " %s\n%!" dep;
	end) extdeps

let print_dep_cval ((specdir,ver,rev,spec) : clone_val) =
  printf "%s %s %d\n%!" specdir ver rev

let print_dep_pval (specdir,vr_opt) =
  match vr_opt with
    | Some (ver,Some rev) ->
	printf "%s %s %d\n%!" specdir ver rev
    | Some (ver,None) ->
	printf "%s %s\n%!" specdir ver
    | None ->
	printf "%s\n%!" specdir

exception Not_found_pkg_target of string

let make pkg_target =
  Params.update_param "pkg-prefix" "";
  Params.update_param "display-command-logs" "false";
  if Sys.file_exists pkg_target then
    begin
      if System.is_directory pkg_target then
	begin
	  let packdir =
	    Filename.dirname (Filename.dirname pkg_target) in
	  let pkgname =
	    Filename.basename (Filename.dirname pkg_target) in
	  let pkg_names =
	    System.with_dir packdir
	      (fun () -> 
		let (data,errors) =
		  Call.read "git" ["tag";"-l"] in
		let nl = Str.regexp "\n" in
		let slash = Str.regexp "/" in
		let split s =
		  match Str.split slash s with
		    | name::_ -> name
		    | [] -> s in
		List.map split
		  (Str.split nl data)) in
	  if List.mem pkgname pkg_names then
	    let depends =
	      tree_of_specdir ~vr:None ~log:false pkg_target in
	    List.iter
	      print_dep_cval
	      (list_of_deptree depends)
	  else
	    let default_branch =
	      Some (Filename.basename pkg_target) in
	    let depends =
	      Packtree.create ~log:false ~default_branch pkg_target in
	    List.iter
	      print_dep_pval
	      (list_of_deptree depends)
	end
      else
	let depends =
	  tree_of_package pkg_target in
	List.iter
	  print_dep_val
	  (list_of_deptree depends)
    end
  else
    raise (Not_found_pkg_target pkg_target)








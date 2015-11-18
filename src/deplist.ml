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
    ["rpmlib";"/bin/bash";"/bin/sh";"perl";"make"]

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

let make pkg_path =
  let depends =
    tree_of_package pkg_path in
  List.iter
    print_dep_val
    (list_of_deptree depends)

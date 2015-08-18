(* Clone support *)

open Printf
open Logger
open Deptree
open Pkgpath
open Clonetree
       
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

let rec download_packages userhost l =
  List.iter 
    (fun e ->
      let src =
	sprintf "%s:%s" userhost e.pkg_path in
      Commands.send_file_over_ssh src ".") l

let clone_packages l =
  List.iter (fun e ->
    let specdir =
      sprintf "./pack/%s/%s" e.pkg_name e.pkg_branch in
    ignore (Package.update ~specdir ~ver:(Some e.pkg_version) ~rev:(Some (string_of_int e.pkg_revision)) ())) l

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

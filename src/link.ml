open Printf
open Pkgpath
open Platform
open Deptree
open Logger

let make ~hard pkg_path =
  let depends =
    Clone.tree_of_package pkg_path in
  let pkg_dir = Filename.dirname pkg_path in
  List.iter (fun e ->
    let name = 
      sprintf "%s-%s-%d.%s.%s.%s"
	e.pkg_name e.pkg_version 
	e.pkg_revision (string_of_platform e.pkg_platform) e.pkg_arch
	e.pkg_extension in
    let file =
      Filename.concat pkg_dir name in
    let do_symlink () =
      Unix.symlink file name in
    let do_hardlink () =
      log_command "ln" ["-f";file;name];
    in
    if hard then
      begin
	try
	  do_hardlink ()
	with exn ->
	  log_message (sprintf "warning: cannot create hardlink: %s by %s\n" name (Printexc.to_string exn));
	  do_symlink ();
      end
    else
      do_symlink ())
  (list_of_deptree depends)

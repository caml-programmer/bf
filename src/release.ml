open Types
open Printf

let reg_pkg_release specdir ver rev =
  let name = "release" in
  Component.with_component_dir ~strict:true (Component.make ~label:(Branch "master") "pack")
    (fun () ->
      let file = 
	sprintf "%s/%s/%s" (Specdir.pkgname specdir) (Specdir.branch specdir) name in
      System.append_write ~file (sprintf "%s %d\n" ver rev);
      Git.git_add file;
      Git.git_commit ~empty:true
	(sprintf "reg pkg release %s %s %s %d" 
	  (Specdir.pkgname specdir) (Specdir.branch specdir) ver rev);
      Git.git_push_cycle ~tags:false ~refspec:None "origin" 5)

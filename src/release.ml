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

(* Read release *)
   
exception Not_found of (string * exn)

let vr_compare a b =
  let r = compare (fst b) (fst a) in
  if r = 0 then
    compare (snd b) (snd a)
  else r

let max_vr l =
  List.hd (List.sort vr_compare l)
     
let read ?(next=false) ?version specdir =
  let with_next n = if next then succ n else n in
  let make s =
    let (ver,rev) =
      let pos = String.index s ' ' in
      String.sub s 0 pos,
      (with_next
	(int_of_string
	  (String.sub s (succ pos) (String.length s - pos - 1))))
    in (ver,rev) in
  let filter (v,r) =
    match version with
      | Some v' -> v' = v
      | None -> true in
  let file = Filename.concat specdir "release" in
  (try
    if Sys.file_exists file then
      let ch = open_in file in
      max_vr (List.filter filter
	(List.map make (System.list_of_channel ch)))
    else raise Exit
  with exn -> 
    raise (Not_found (specdir,exn)))
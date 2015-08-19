(* Fix build support *)

open Types
open Deptree
open Printf
open Changelog

type pkg = string * version * revision
type task_id = string
type fixmap =
    (pkg * task_id list) list

let non_first_build (rev_a,rev_b) call =
  let first_rev =
    snd (vr_of_rev rev_a) = 0 in
  if not first_rev then
    call ()

let make specdir rev_a rev_b =
  let fixmap = ref [] in
  non_first_build (rev_a,rev_b)
    (fun () ->
      try
	let tree_a = Clonetree.tree_of_specdir ~log:false ~vr:(Some (vr_of_rev rev_a)) specdir in
	let tree_b = Clonetree.tree_of_specdir ~log:false ~vr:(Some (vr_of_rev rev_b)) specdir in
	let depends_a = List.map (fun (p,v,r,s) -> p,(v,r)) (list_of_deptree tree_a) in
	let depends_b = List.map (fun (p,v,r,s) -> p,(v,r)) (list_of_deptree tree_b) in
	List.iter
	  (fun (pkgname_b,(ver_b,rev_b)) ->
	    (try
	      let (ver_a,rev_a) =
		List.assoc pkgname_b depends_a in
	      let pkgname = Specdir.pkgname pkgname_b in
	      let tag_a = sprintf "%s/%s-%d" pkgname ver_a rev_a in
	      let tag_b = sprintf "%s/%s-%d" pkgname ver_b rev_b in
	      if tag_a <> tag_b then
		begin
		  let composite =
		    Filename.concat pkgname_b "composite" in
		  let tasks = 
		    List.flatten
		      (List.map 
			(Component.changelog_tasks tag_a tag_b)
			(List.filter (fun c -> c.name <> (Params.get_param "pack") && c.pkg = None && (not c.nopack))
			  (Composite.components composite))) in
		  fixmap := (((pkgname,ver_b,rev_b), tasks)::!fixmap)
		end;
	    with Not_found -> ()))
	  depends_b
      with exn ->
	Logger.log_message (sprintf "=> make-fix-map by %s, (current-dir %s)\n" (Printexc.to_string exn) (Sys.getcwd ())));
  !fixmap

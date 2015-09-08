(* changelogNG.ml *)

open Printf
       
let changelog specdir rev_a rev_b =
  let tree_a = Clonetree.tree_of_specdir ~log:false ~vr:(Some (Changelog.vr_of_rev rev_a)) specdir in
  let tree_b = Clonetree.tree_of_specdir ~log:false ~vr:(Some (Changelog.vr_of_rev rev_b)) specdir in

  print_endline "\nTREE - A:\n";

  List.iter (fun (pkgname,ver,rev,spec) ->
	     printf "name, ver, rev: %s %s %d\n\n" pkgname ver rev)
	    (Deptree.list_of_deptree tree_a);
  
  print_endline "\nTREE - B:\n";

  List.iter (fun (pkgname,ver,rev,spec) ->
	     printf "name, ver, rev: %s %s %d\n\n" pkgname ver rev)
	    (Deptree.list_of_deptree tree_b);

  print_endline "\n"

		

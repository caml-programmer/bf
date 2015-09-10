(* changelogNG.ml *)

open Output

let changelog specdir rev_a rev_b =
  let tree_a = Clonetree.tree_of_specdir ~log:false ~vr:(Some (Changelog.vr_of_rev rev_a)) specdir in
  let tree_b = Clonetree.tree_of_specdir ~log:false ~vr:(Some (Changelog.vr_of_rev rev_b)) specdir in
  msg "changelog" "debug" "TREE A: --------------------";
  msg "changelog" "debug" (Clonetree.string_of_clone_tree tree_a);
  msg "changelog" "debug" "TREE B: --------------------";
  msg "changelog" "debug" (Clonetree.string_of_clone_tree tree_b);
  Check.pack_component ()


let test () =
  msg "changelog" "low" "fucking holy shit!"
		

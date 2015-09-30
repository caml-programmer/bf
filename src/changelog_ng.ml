(* changelogNG.ml *)

open Component
open Output

let make ?(recursive=false) pkg_name version begin_rev end_rev =
  ignore begin (string_of_bool recursive)
	       ^ pkg_name ^ version
	       ^ (string_of_int begin_rev)
	       ^ (string_of_int end_rev)
	 end;
  let msg loglevel text = msg "Changelog_ng.make" loglevel text in
  let begin_tag = Tag.make pkg_name version begin_rev in
  let end_tag = Tag.make pkg_name version end_rev in
  let specdir = Specdir.specdir_by_version pkg_name version in
  msg "always" ("tag-a: " ^ begin_tag);
  msg "always" ("tag-b: " ^ end_tag);
  msg "always" ("specdir: " ^ specdir);
  let begin_tree =
    Clonetree.tree_of_specdir ~newload:true ~log:false
			      ~vr:(Some (Release.of_ver_rev version begin_rev)) specdir in
  let end_tree =
    Clonetree.tree_of_specdir ~newload:true ~log:false
			      ~vr:(Some (Release.of_ver_rev version end_rev)) specdir in
  msg "debug" "TREE BEGIN: --------------------";
  msg "debug" (Clonetree.string_of_clone_tree begin_tree);
  msg "debug" "TREE END: --------------------";
  msg "debug" (Clonetree.string_of_clone_tree end_tree)
  
       
(* данная функция выводит changelog пакета и всех его зависимостей *)       
let changelog_specdir specdir rev_a rev_b =
  let tree_a = Clonetree.tree_of_specdir ~log:false ~vr:(Some (Changelog.vr_of_rev rev_a)) specdir in
  let tree_b = Clonetree.tree_of_specdir ~log:false ~vr:(Some (Changelog.vr_of_rev rev_b)) specdir in
  msg "changelog" "debug" "TREE A: --------------------";
  msg "changelog" "debug" (Clonetree.string_of_clone_tree tree_a);
  msg "changelog" "debug" "TREE B: --------------------";
  msg "changelog" "debug" (Clonetree.string_of_clone_tree tree_b);
  Check.pack_component ();
  let mkdep (specdir,ver,rev,spec) = specdir,(ver,rev) in
  let depends_a = List.map mkdep (Deptree.list_of_deptree ~add_parent:true tree_a) in
  let depends_b = List.map mkdep (Deptree.list_of_deptree ~add_parent:true tree_b) in

  print_endline "\nCHANGELOG:\n";

  List.iter
    (fun (specdir_b,(ver_b,rev_b)) ->
     (try
	let (ver_a,rev_a) =
	  List.assoc specdir_b depends_a in
	let pkgname = Specdir.pkgname specdir_b in
	let tag_a = Tag.mk (pkgname, ver_a, rev_a) in
	let tag_b = Tag.mk (pkgname, ver_b, rev_b) in
	if tag_a <> tag_b then
	  Printf.printf "# %s %s %d -> %s %d\n%!" specdir_b ver_a rev_a ver_b rev_b;
	let composite =
	  Filename.concat specdir_b "composite" in
	if tag_a <> tag_b then
	  List.iter (List.iter (Printf.printf "%s\n%!"))
		    (List.map (Component.changelog tag_a tag_b)
			      (List.filter (fun c -> c.name <> (Params.get_param "pack") && c.pkg = None && (not c.nopack))
					   (Composite.components composite)))
       with Not_found ->
	 Printf.printf "+ %s %s %d\n%!" specdir_b ver_b rev_b))
    depends_b;

  List.iter
    (fun (specdir_a,(ver_a,rev_a)) ->
      if not (List.mem_assoc specdir_a depends_b) then
	Printf.printf "- %s %s %d\n%!" specdir_a ver_a rev_a)
    depends_a

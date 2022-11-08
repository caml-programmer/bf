open Component
open Chroot
       
let test name call =
  Printf.printf "Test %s: %b\n" name (call ())

let write_lines file lines =
  let buf = Buffer.create 32 in
  let add = Buffer.add_string buf in
  let out s = add s; add "\n"; in
  List.iter out lines;
  System.write
    (Buffer.contents buf)
    file

let chroot_buildpkg () =
  Check.pack_component ();

  let os = Platform.Linux in
  let platform = Platform.Rhel in
  let pkgname = "jet-racket5" in
  let version = "14.0.0" in
  let pkgspec = Spectype.newload ~os ~platform pkgname version in
  
  Chroot.buildpkg ~os ~platform pkgspec;
  ()
    
let test_packpkg () =
  let os = Platform.Linux in
  let platform = Platform.Rhel in
  let chroot_name = "centos--jet-racket5" in
  let pkgname = "jet-racket5" in
  let version = "14.0.0" in
  let pkgspec = Spectype.newload ~os ~platform pkgname version in
  
  Chroot.pack ~os ~platform chroot_name pkgspec;
  ()

let make_chroot chroot_name platform =
    Chroot.make chroot_name platform

(* test-deptree *)
let depgraph pkgname version revision_opt =
  let depgraph = Depgraph.of_pkg pkgname version revision_opt in
  print_endline (Depgraph.string_of_deptree depgraph)

(* test-buildtree *)
let buildgraph pkgname version revision_opt =
  let depgraph = Depgraph.of_pkg ~use_builddeps:true pkgname version revision_opt in
  print_endline (Depgraph.string_of_deptree ~use_builddeps:true depgraph)

(* test-deplist *)
let deplist pkgname version =
  let depgraph = Depgraph.of_pkg pkgname version None in
  print_endline (Depgraph.string_of_deplist depgraph)

(* test-rec-complist *)
let rec_complist pkgname version =
  let depgraph = Depgraph.of_pkg pkgname version None in
  print_endline (Depgraph.string_of_component_list depgraph)

let depcomps = rec_complist
		
(* test-depload *)
let depload file =
  let deplist_v1 = Spectype.depload file in
  let deplist_v2 = Spectype.depload_v2_new file in
  print_endline ("---------- DEPS V1 ----------");
  print_endline (Output.string_of_string_list
		   (List.map Spectype.string_of_platform_depend deplist_v1));
  print_endline ("---------- DEPS V2 ----------");
  print_endline (Output.string_of_string_list
		   (List.map Spectype.string_of_platform_depend deplist_v2))

(* load-chroot-cfg *)
let load_chroot_cfg () =
  let chroot = Chroot.load_chroot_cfg "rhel7-for-java" Platform.Rhel in
  print_endline (string_of_chroot chroot)
		
(* depgraph *)
(*let draw_depgraph pkgname version =
  let depgraph = Depgraph.of_pkg pkgname version None in
  Depgraph.draw depgraph "graph.dot"*)
let draw_depgraph pkgname version =
  let depgraph = Depgraph2.of_pkg pkgname version in
  Depgraph2.draw ~local_only:false depgraph

(* buildgraph *)
let draw_buildgraph pkgname version =
  let depgraph = Depgraph.of_pkg ~use_builddeps:true pkgname version None in
  Depgraph.draw ~use_builddeps:true depgraph "graph.dot"

(* subtree-buildgraph*)
let draw_subtree_buildgraph pkgname version =
  let depgraph = Depgraph2.subtree_buildgraph ~local_only:true pkgname version in
  Depgraph2.draw ~local_only:true depgraph


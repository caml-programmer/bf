open Component

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
  let platform = Platform.Cent6 in
  let chroot_name = "centos" in
  let pkgname = "jet-racket5" in
  let version = "14.0.0" in
  let pkgspec = Spectype.newload ~os ~platform pkgname version in
  
  Chroot.buildpkg ~os ~platform chroot_name pkgspec;
  ()

let test_packpkg () =
  let os = Platform.Linux in
  let platform = Platform.Cent6 in
  let chroot_name = "centos--jet-racket5" in
  let pkgname = "jet-racket5" in
  let version = "14.0.0" in
  let pkgspec = Spectype.newload ~os ~platform pkgname version in
  
  Chroot.pack ~os ~platform chroot_name pkgspec;
  ()

let make_chroot chroot_name platform =
    Chroot.make chroot_name platform

let depgraph pkgname version revision_opt =
  let depgraph = Depgraph.of_pkg pkgname version revision_opt in
  print_endline (Depgraph.string_of_deptree depgraph)

let depload file =
  let deplist_v1 = Spectype.depload file in
  let deplist_v2 = Spectype.depload_v2_new file in
  print_endline ("---------- DEPS V1 ----------");
  print_endline (Output.string_of_string_list
		   (List.map Spectype.string_of_platform_depend deplist_v1));
  print_endline ("---------- DEPS V2 ----------");
  print_endline (Output.string_of_string_list
		   (List.map Spectype.string_of_platform_depend deplist_v2))



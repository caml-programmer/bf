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

let chroots () =
  Chroot.make "centos" Platform.Cent6  (*;
  Chroot.buildpkg "centos" "jet-racket5" "14.0.0"*)

let depgraph pkgname version revision =
  let depgraph = Depgraph.of_pkg pkgname version revision in
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
  

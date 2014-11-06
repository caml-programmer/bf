open Test
open Types
open Spectype
open Printf

type package_info = {
  name: string;
  composite: component list;
  depends: Depends.depends;
  version: version;
  release: (version * revision) list
}

let home () = ".test-fork"
  
let with_home f =
  System.with_dir (home ()) f

let remakedir x =
  if Sys.file_exists x then
    System.remove_directory x;
  Unix.mkdir x 0o755

let create_test_home () =
  remakedir (home ())

let create_bf_params () =
  write_lines ".bf-params"
    [
      "top-dir top";
      "git-url projects";
      "dev-dir dev";
      "dest-dir dest";
      "log-level low";
      "smtp-notify-email root@localhost";
      "smtp-server localhost";
      "pkg-storage localhost";
      "pkg-branch vendor/pkg-branch";
      "plugins-dir pack";
      "autopkg true";
      "use-external false";
      (*out "content-owner root";*)
    ]

let create_package_info pi =
  Unix.mkdir pi.name 0o755;
  System.with_dir pi.name
    (fun () ->
      Version.write "version" pi.version;
      Composite.write "composite" pi.composite;
      Release.write "release" pi.release;
      Depends.write "depends" pi.depends)

let create_component name =
  System.with_dir "../projects"
    (fun () ->
      printf "Create component(%s): %s\n%!" (Sys.getcwd ()) name;
      Unix.mkdir name 0o755;
      System.with_dir name
	(fun () ->
	  Git.git_init ();
	  System.write "abc\n" "test";
	  Git.git_add "test";
	  Git.git_commit "init"))

let create_pack_packages () =
  let mkc name =
    create_component name;
    Component.make
      ~label:(Branch "master")
      ~rules:None
      ~nopack:false
      ~pkg:None name in
  let p1 =
    let composite =
      [
	mkc "pack";
	mkc "component_a";
	mkc "component_b";
	mkc "component_c";
      ]
    in
    let depends = [
      "linux", [
	"vendor-package-level-two-a", Some (Pkg_last, "1.0"),   Some "vendor package level two a";
	"vendor-package-level-two-b", Some (Pkg_last, "2.2.2"), Some "vendor package level two b";
	"vendor-package-level-two-c", Some (Pkg_eq, "3.0"),     Some "vendor package level two c";
	"vendor-package-level-two-d", Some (Pkg_ge, "4.0.1"),   Some "vendor package level two d";
      ]
    ] in
    let release = [
      "1.0",0;
      "2.0",0;
      "3.1.2",0;
      "5.0.4.1.3",0
    ] in
    {
      name = "vendor-package-level-one";
      composite = composite;
      depends = depends;
      version = "2.0";
      release = release
    } in
  List.iter create_package_info [p1]

let create_pack () =
  Unix.mkdir "pack" 0o755;
  Unix.mkdir "projects" 0o755;
  System.with_dir "pack"
    create_pack_packages
    
let create_infrastructure () =
  printf "Current directory: %s\n" (Sys.getcwd ());
  create_test_home ();
  with_home 
    (fun () ->
      create_bf_params ();
      create_pack ())

let fork () =
  create_infrastructure ();
  true

let run () =
  test "fork" fork

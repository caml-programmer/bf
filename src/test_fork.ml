open Test
open Types
open Spectype
open Printf

type package_info = {
  name: string;
  branch: string;
  composite: component list;
  depends: Depends.depends;
  version: version;
  release: (version * revision) list
}

let home () = "test-fork"
  
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
      "dest-dir " ^ (Filename.concat (Sys.getcwd ()) "dest");
      "log-level low";
      "smtp-notify-email root@localhost";
      "smtp-server localhost";
      "pkg-storage localhost";
      "pkg-branch vendor/pkg-branch";
      "plugins-dir pack";
      "autopkg true";
      "use-external false";
      "pkg-prefix vendor";
      (*out "content-owner root";*)
    ]

let create_package_info pi =
  let bdir =
    Filename.concat pi.name pi.branch in
  Unix.mkdir pi.name 0o755;
  Unix.mkdir bdir 0o755;
  System.with_dir bdir
    (fun () ->
      Version.write "version" pi.version;
      Composite.write "composite" pi.composite;
      Release.write "release" pi.release;
      Depends.write "depends" pi.depends)

let create_component name =
  if name <> "pack"  then
    begin
      printf "Create component(%s): %s\n%!" (Sys.getcwd ()) name;
      Unix.mkdir name 0o755;
      System.with_dir name
	(fun () ->
	  Git.git_init ();
	  System.write "abc\n" "test";
	  Git.git_add "test";
	  Git.git_commit "init")
    end

let create_pack_packages () =
  let mkc name =
    System.with_dir ".."
      (fun () ->
	create_component name;
	Component.make
	  ~label:(Branch "master")
	  ~rules:None
	  ~nopack:false
	  ~pkg:None name) in
  let p1 =
    let composite =
      [
	mkc "pack";
	mkc "component_a";
	(*mkc "component_b"; mkc "component_c";*)
      ]
    in
    let depends = [
      "linux", [
	"vendor-project-package-level-two-a", Some (Pkg_last, "1.0"),   Some "vendor package level two a";
(*	"vendor-project-package-level-two-b", Some (Pkg_last, "2.2.2"), Some "vendor package level two b";
	"vendor-project-package-level-two-c", Some (Pkg_eq, "3.0"),     Some "vendor package level two c";
	"vendor-project-package-level-two-d", Some (Pkg_ge, "4.0.1"),	Some "vendor package level two d";
*)
      ]
    ] in
    let release = [
      "1.0",0;
(*      "2.0",0;
      "3.1.2",0;
      "5.0.4.1.3",0*)
    ] in
    {
      name = "vendor-project-package-level-one";
      branch = "devel";
      composite = composite;
      depends = depends;
      version = "2.0";
      release = release
    } in

  let p2 = 
    let composite =
      [
	mkc "pack";
	mkc "component_b";
      ]
    in
    let depends = [ "linux", [] ] in
    let release = [ "1.0",0 ] in
    {
      name = "vendor-project-package-level-two-a";
      branch = "devel";
      composite = composite;
      depends = depends;
      version = "2.0";
      release = release
    } in
  List.iter create_package_info [p1;p2]

let create_pack () =
  Unix.mkdir "projects" 0o755;
  System.with_dir "projects"
    (fun () ->
      Unix.mkdir "pack" 0o755;
      System.with_dir "pack" create_pack_packages;
      System.write
	"(define (build args) ())\n(define (install args) ())\n" "pack/.bf-rules";
      System.with_dir "pack"
	(fun () ->
	  Git.git_init ();
	  Git.git_add ".";
	  Git.git_commit "init";

	  let cmd = "git config receive.denyCurrentBranch ignore" in
	  let rc = Sys.command cmd in
	  if rc <> 0 then
	    failwith cmd))
    
let create_destdir () =
  remakedir "dest";
  remakedir "dest/top"

let create_infrastructure () =
  printf "Current directory: %s\n" (Sys.getcwd ());
  create_test_home ();
  with_home
    (fun () ->
      create_destdir ();
      create_bf_params ();
      create_pack ())

let stage = ref 0;;

let check_result label =
  printf "\n* [%s] %d **************************************\n" label !stage;
  if System.is_directory "pack" then
    System.with_dir "pack"
      (fun () ->
	List.iter
	(fun package ->
	  if (System.is_directory package) then
	    begin
	      System.with_dir package
		(fun () ->
		  List.iter
		  (fun branch ->
		    if System.is_directory branch then
		      System.with_dir branch 
			(fun () ->
			  let file =
			    sprintf "%s/%s" (Sys.getcwd ()) "release" in
			  if System.is_regular file then
			    List.iter
			      (fun (ver,rev) -> printf "%s - %s %d\n%!" file ver rev)
			      (Release.read file)))
		  (System.list_of_directory "."))
	    end)
	(System.list_of_directory "."));
  printf "* [%s] %d **************************************\n\n%!" label !stage;
  if label = "after" then
    incr stage

let do_fork forkmap =
  with_home
    (fun () ->
      ignore(Params.reread_params ());
      printf "Current directory: %s\n" (Sys.getcwd ());
      Component.prepare (Component.make "pack");
      List.iter
	(fun (p,b) ->
	  check_result "before";
	  Fork.make ~interactive:false ("./pack/" ^ p) b;
	  check_result "after")
	forkmap);
  true

let fork () =
  create_infrastructure ();
  
  let forkmap = [
    "vendor-project-package-level-one/devel",         "project-1.0";
    "vendor-project-package-level-one/project-1.0",   "project-1.1";
    "vendor-project-package-level-one/project-1.1",   "project-1.2";
    "vendor-project-package-level-one/project-1.2",   "project-1.2.1";
    "vendor-project-package-level-one/project-1.2.1", "project-2.0.0";
  ] in
  
  do_fork forkmap

let run () =
  test "fork" fork


(* Buildfarm for GIT-repository *)

open Component

(* Locking *)

let fd_lock =
  lazy
    (Unix.openfile (Params.get_param "lock-file")
      [Unix.O_CREAT;Unix.O_RDONLY;Unix.O_WRONLY] 0o644)
;;

let lock () =
  print_string "locking...";
  Unix.lockf (Lazy.force fd_lock) Unix.F_LOCK 1;
  print_endline "ok"

let unlock () =
  print_string "unlocking...";
  Unix.lockf (Lazy.force fd_lock) Unix.F_ULOCK 1;
  Unix.close (Lazy.force fd_lock);
  print_endline "ok"

(* No lock *)

let do_lock =
  try
    ignore(Sys.getenv "BF_NO_LOCK");
    false
  with Not_found -> true

let with_lock f =
  if do_lock then
    begin
      lock (); f (); unlock ()
    end
  else
    f ()

type analyze_result =
  | Is_components of component list
  | Is_component_with_label of component
  | Is_composite of string
  | Is_composite_with_tag of (string * string)

let usage () =
  print_endline "Usage: bf (prepare|update|forward|[re]build|[re]install|status) <components>";
  print_endline "   or: bf (prepare|update|forward|[re]build|[re]install|status) <component> [branch <branch> | tag <tag>]";
  print_endline "   or: bf (prepare|update|forward|[re]build|[re]install|status) <composite> [<tag>]";
  print_endline "   or: bf (diff|changelog) <composite> <tag-a> <tag-b> [compact]";
  print_endline "   or: bf (diff|changelog) <specdir> <rev-a> <rev-b>";
  print_endline "   or: bf review <composite> <since-date>";
  print_endline "   or: bf pack <specdir> <version> <release>";
  print_endline "   or: bf update <specdir> [lazy] [<version>] [<release>]";
  print_endline "   or: bf upgrade <specdir> [lazy|complete|full] [<branch>]";
  print_endline "   or: bf fork <specdir> <new-branch> [<single-rev-depth>]";
  print_endline "   or: bf clone <ssh-user>@<ssh-host> <pkg-path> [overwrite|depends|packages]";
  print_endline "   or: bf clone <specdir> [overwrite] [norec] [<ver> <rev>]";
  print_endline "   or: bf snapshot <specdir> [<composite>]";
  print_endline "   or: bf link <pkg-path> [symlink]";
  print_endline "   or: bf top <specdir>";
  print_endline "   or: bf graph <specdir> [<ver> <rev>]";
  print_endline "   or: bf basegraph <specdir> [hard|soft]";
  print_endline "   or: bf usergraph <specdir>";
  print_endline "   or: bf info <specdir>";
  print_endline "   or: bf tag <composite> <tag>";
  print_endline "   or: bf make (build|install) [<name>]";
  print_endline "   or: bf versions <pkgdir>";
  print_endline "   or: bf search <commit-id>";
  print_endline "   or: bf shell";
  print_endline "   or: bf clean";
  print_endline "   or: bf droptags <lifetime>";
  print_endline "   or: bf log";
  print_endline "   or: bf tests";
  print_endline "   or: bf checknode <smtp-server>[:<smtp-port>] <e-mail-list>";
  print_endline "   or: bf test-deptree <pkg-name> <version> <revision>";
  exit 1

let make_int s =
  try int_of_string s with _ -> usage ()

let make_rules () =
  try
    Some (Sys.getenv "RULES")
  with Not_found -> None
    
let analyze_arguments () =
  match Array.length Sys.argv with
    | 1 | 2 -> usage ()
    | 3 ->
	let two = Sys.argv.(2) in
	if Sys.file_exists two && System.is_regular two then
	  Is_composite two
	else
	  Is_components [ Component.make two ]
    | 4 ->
	let two = Sys.argv.(2) in
	if Sys.file_exists two && System.is_regular two then
	  Is_composite_with_tag (two,Sys.argv.(3))
	else
	  (match  Sys.argv.(3) with
	    | "branch" | "tag" -> usage ()
	    |  _ -> 
		 Is_components 
		  [ Component.make Sys.argv.(2); Component.make Sys.argv.(3) ])
    | 5 ->
	(match  Sys.argv.(3) with
	  | "branch" ->
	      Is_component_with_label
		{ 
		  name = Sys.argv.(2); 
		  label = (Branch Sys.argv.(4));
		  pkg = None; 
		  rules = make_rules ();
		  nopack = false;
		  forkmode = Branching;
		}
	  | "tag" ->
	      Is_component_with_label
		{ 
		  name = Sys.argv.(2); 
		  label = (Tag Sys.argv.(4)); 
		  pkg = None; 
		  rules = make_rules (); 
		  nopack = false; 
		  forkmode = Branching
		}
	  |  _ ->
	       Is_components
		[ 
		  Component.make Sys.argv.(2);
		  Component.make Sys.argv.(3);
		  Component.make Sys.argv.(4);
		])
    | _ ->
	Is_components
	  (List.map Component.make (List.tl (List.tl (Array.to_list Sys.argv))))

type teleport_mode =
  | Goto_bf_rules
  | Goto_bf_params

exception Rules_not_found
exception Params_not_found

let with_teleport mode f =
  let location =
    Sys.getcwd () in
  let new_location =
    match mode with
      | Goto_bf_rules ->
	  (match System.up_search ~default:None ".bf-rules" with
	    | None -> raise Rules_not_found
	    | Some x -> Filename.dirname x)
      | Goto_bf_params ->
	  (match System.up_search ~default:None ".bf-params" with
	    | None -> raise Params_not_found
	    | Some x -> Filename.dirname x)
  in
  let goto x =
    Sys.chdir new_location;
    Params.update_param "start-dir" new_location in
  goto new_location; f (); goto location

let main () =
  Check.pack_component ();
  let len = Array.length Sys.argv in
    if len > 1 then
      let action = Sys.argv.(1) in
      let analyze () =
	match analyze_arguments () with
	  | Is_components components ->
	      (match action with
		| "prepare"   -> Components.prepare   components
		| "build"     -> Components.build     components
		| "rebuild"   -> Components.rebuild   components
		| "install"   -> ignore(Components.install components)
		| "reinstall" -> Components.reinstall components
		| "update"    -> ignore(Components.update components)
		| "forward"   -> Components.forward   components
		| "status"    -> Components.status    components
		| _           -> usage ())
	  | Is_component_with_label component ->
	      (match action with
		| "prepare"   -> Components.prepare   [component]
		| "build"     -> Components.build     [component]
		| "rebuild"   -> Components.rebuild   [component]
		| "install"   -> ignore(Components.install [component])
		| "reinstall" -> Components.reinstall [component]
		| "update"    -> ignore(Components.update [component])
		| "forward"   -> Components.forward   [component]
		| "status"    -> Components.status    [component]
		| _           -> usage ())
	  | Is_composite composite ->
	      Params.set_composite_mode ();
	      (match action with
		| "prepare"   -> Composite.prepare composite
		| "build"     -> Composite.build composite
		| "rebuild"   -> Composite.rebuild composite
		| "install"   -> ignore(Composite.install composite)
		| "reinstall" -> Composite.reinstall composite
		| "update"    -> ignore(Composite.update composite)
		| "forward"   -> Composite.forward composite
		| "status"    -> Composite.status composite
		| _           -> usage ())
	  | Is_composite_with_tag (composite,tag) ->
	      Params.set_composite_mode ();
	      (match action with
		| "prepare"   -> Composite.prepare   ~tag composite
		| "build"     -> Composite.build     ~tag composite
		| "rebuild"   -> Composite.rebuild   ~tag composite
		| "install"   -> ignore(Composite.install  ~tag composite)
		| "reinstall" -> Composite.reinstall ~tag composite
		| "update"    -> ignore(Composite.update ~tag composite)
		| "forward"   -> Composite.forward   ~tag composite
		| "status"    -> Composite.status    ~tag composite
		| _           -> usage ())
      in match action with
	| "pack" ->
	    if len = 5 then
	      let specdir = Sys.argv.(2) in
	      let version = Sys.argv.(3) in
	      let release = Sys.argv.(4) in
	      Pkgbuild.build_package_file
		(specdir,version,release)
	    else usage ()
	| "update" ->
	    if len > 2 then
	      let specdir = Sys.argv.(2) in
	      let version = Filename.concat specdir "version" in
	      let composite = Filename.concat specdir "composite" in
	      let (lazy_mode,lazy_pos) =
		if      len > 3 && Sys.argv.(3) = "lazy" then true,3
		else if len > 4 && Sys.argv.(4) = "lazy" then true,4
		else if len > 5 && Sys.argv.(5) = "lazy" then true,5
		else false, 2
	      in
	      let ver =
		let pos =
		  if lazy_pos = 3 then 4 else 3 in
		if len > pos then
		  Some Sys.argv.(pos)
		else None
	      in
	      let rev =
		let pos =
		  if lazy_pos = 3 || lazy_pos = 4 then 5 else 4 in
		if len > pos then
		  Some Sys.argv.(pos)
		else None
	      in
	      if Sys.file_exists version then
		with_lock (fun () ->
		  ignore (Package.update ~specdir ~lazy_mode ~ver ~rev ()))
	      else
		analyze ()
	    else
	      analyze ()
	| "log" ->
	    if len = 2 then
	      Commands.log_viewer ()
	    else usage ()
	| "clone" ->
	    if Sys.file_exists Sys.argv.(2) then
	      begin
		let check_rec s = if s = "norec" then false else usage () in
		let check_over s = if s = "overwrite" then true else usage () in
		let is_opt s = s = "overwrite" || s = "norec" in
		let (recursive,overwrite,vr) =
		  if len = 3 then
		    (true,false,None)
		  else if len = 4 then
		    (match Sys.argv.(3) with
		      | "overwrite" -> (true,true,None)
		      | "norec" -> (false,false,None)
		      | _ -> usage ())
		  else if len = 5 then
		    (match Sys.argv.(3) with
		      | "overwrite" -> (check_rec Sys.argv.(4),true,None)
		      | "norec" -> (false,check_over Sys.argv.(4),None)
		      | _ -> (true,false,Some (Sys.argv.(3), make_int Sys.argv.(4))))
		  else if len = 6 then
		    if is_opt Sys.argv.(3) && is_opt Sys.argv.(4) then
		      (false,true,None)
		    else
		      (match Sys.argv.(3) with
			| "overwrite" -> (true,true,Some (Sys.argv.(4), make_int Sys.argv.(5)))
			| "norec" -> (false,false,Some (Sys.argv.(4), make_int Sys.argv.(5)))
			| _ -> (true,false,Some (Sys.argv.(4), make_int Sys.argv.(5))))
		  else if len = 7 then
		    (match Sys.argv.(3) with
		      | "overwrite" -> (check_rec Sys.argv.(4),true,Some (Sys.argv.(5), make_int Sys.argv.(6)))
		      | "norec" -> (false,check_over Sys.argv.(4),Some (Sys.argv.(5), make_int Sys.argv.(6)))
		      | _ -> usage ())
		  else
		    usage ()
		in with_lock (fun () -> Clone.make ~vr ~recursive ~overwrite Sys.argv.(2))
	      end
	    else
	      begin
		if len = 4 then
		  with_lock (fun () -> Clone.by_pkgfile Sys.argv.(2) Sys.argv.(3) "default")
		else
		  if len = 5 then
		    with_lock (fun () -> Clone.by_pkgfile Sys.argv.(2) Sys.argv.(3) Sys.argv.(4))
		  else
		    usage ()
	      end
	| "top" ->
	    if Sys.file_exists Sys.argv.(2) then
	      Top.make Sys.argv.(2)
	    else usage ()
	| "upgrade" ->
	    let (upgrade_mode,default_branch) =
	      match len with
		| 3 -> Upgrade.Default, None
		| 4 ->
		    (match Sys.argv.(3) with
		      | "lazy"     -> (Upgrade.Lazy,None)
		      | "complete" -> (Upgrade.Complete,None)
		      | "full"     -> (Upgrade.Full,None)
		      | _          -> (Upgrade.Default,Some Sys.argv.(3)))
		| 5 ->
		    (match Sys.argv.(3) with
		      | "lazy"     -> (Upgrade.Lazy,Some Sys.argv.(4))
		      | "complete" -> (Upgrade.Complete,Some Sys.argv.(4))
		      | "full"     -> (Upgrade.Full,Some Sys.argv.(4))
		      | _          ->
			  (match Sys.argv.(4) with
			    | "lazy"     -> (Upgrade.Lazy,Some Sys.argv.(3))
			    | "complete" -> (Upgrade.Complete,Some Sys.argv.(3))
			    | "full"     -> (Upgrade.Full,Some Sys.argv.(3))
			    | _ -> usage ()))
		| _ -> usage ()
	    in
	    with_lock
	      (fun () ->
		Upgrade.make Sys.argv.(2) upgrade_mode default_branch)
	| "fork" ->
	    if len <> 4 then
	      if len <> 5 then
		usage ()
	      else
		with_lock (fun () ->
		  Fork.make ~depth:(make_int Sys.argv.(4)) Sys.argv.(2) Sys.argv.(3))
	    else
	      with_lock (fun () ->
		Fork.make Sys.argv.(2) Sys.argv.(3))
	| "graph" ->
	    if len <> 3 then
	      if len <> 5 then
		usage ()
	      else
		with_lock (fun () ->
		  Graph.monograph ~ver:Sys.argv.(3) ~rev:(make_int Sys.argv.(4)) Sys.argv.(2))
	    else
	      with_lock (fun () -> Graph.monograph Sys.argv.(2))
	| "basegraph" ->
	    let check_mode = function
	      | "soft" -> "soft"
	      | "hard" -> "hard"
	      |  _     -> usage ()
	    in
	    if len <> 3 then
	      if len <> 4 then
		usage ()
	      else
		with_lock (fun () -> Graph.basegraph Sys.argv.(2) (check_mode Sys.argv.(3)))
	    else
	      with_lock (fun () -> Graph.basegraph Sys.argv.(2) "full")
	| "usergraph" ->
	    if len = 3 then
	      with_lock (fun () -> Graph.usergraph Sys.argv.(2))
	    else usage ()
	| "info" ->
	    if len = 3 then
	      with_lock (fun () -> Graph.make_info_table Sys.argv.(2))
	    else usage ()
	| "tag" ->
	    if len = 4 then
	      with_lock (fun () ->
		Composite.tag Sys.argv.(2) Sys.argv.(3))
	    else usage ()
	| "review" ->
	    if len = 4 then
	      Composite.review Sys.argv.(2) Sys.argv.(3)
	    else usage ()
	| "diff" ->
	    Params.disable_display_logs ();
	    if len = 5 then
	      if Filename.basename Sys.argv.(2) = "composite" then
		Composite.diff Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	      else
		Changelog.diff Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	    else usage ()
	| "changelog" ->
	    Params.disable_display_logs ();
	    if len = 5 || len = 6 then
	      if Filename.basename Sys.argv.(2) = "composite" then
		Composite.changelog ~interactive:true
		  ~compact:(len=6) Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	      else
		Changelog.make Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	    else usage ()
	| "diff-ng" -> ()
	| "changelog-ng" -> Changelog_ng.changelog_specdir Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	| "link" ->
	    if len = 3 then
	      Link.make ~hard:true Sys.argv.(2)
	    else
	      if len = 4 && Sys.argv.(3) = "symlink" then
		Link.make ~hard:false Sys.argv.(1)
	      else usage ()
	| "snapshot" ->
	    (match len with
	      | 3 -> Snapshot.make Sys.argv.(2)
	      | 4 -> 
		  if Sys.file_exists Sys.argv.(3) then
		    Snapshot.make ~composite:(Some Sys.argv.(3)) Sys.argv.(2)
		  else usage ()
	      | _ -> usage ())
	| "shell" ->
	    Scheme.shell ()
	| "clean" ->
	    Clean.packages ()
	| "make" ->
	    with_teleport Goto_bf_rules
	      (fun () ->
		Params.update_param "log-level" "high";
		Params.update_param "display-command-logs" "true";
		Params.update_param "plugins-dir" (Printf.sprintf "../%s" (Params.get_param "pack"));
		Params.update_param "orig-top-dir" (Params.get_param "top-dir");
		Params.update_param "install-dir" (Params.make_install_dir ());
		match len with
		  | 2 ->
		      Rules.build_rules None;
		      Rules.install_rules ~check_build:false None
		  | 3 ->
		      (match Sys.argv.(2) with
			| "build" -> Rules.build_rules None
			| "install" -> Rules.install_rules ~check_build:false None
			| _ -> usage ())
		  | 4 ->
		      (match Sys.argv.(2) with
			| "build" -> Rules.build_rules (Some Sys.argv.(3))
			| "install" -> Rules.install_rules ~check_build:false (Some Sys.argv.(3))
			| _ -> usage ())
		  | _ ->
		      usage ())
	| "versions" ->
	    if len <> 3 then
	      usage ()
	    else
	      with_lock (fun () -> Versions.last Sys.argv.(2))
	| "search" ->
	    if len = 3 then
	      with_lock (fun () ->
		Search.commit_id Sys.argv.(2))
	    else usage ()
	| "droptags" ->
	    if len > 2 then
	      Clean.droptags Sys.argv.(2)
	    else usage ()
	| "tests" ->
	   Tests.run ()
	| "checknode" ->
	    begin
	      let (smtp_server, smtp_port, emails) = 
		match Array.to_list Sys.argv with
		  | _::_::server_info::emails ->
		      (match Str.split (Str.regexp ":") server_info with
			| server::port::_ ->
			    (try
			      (server,int_of_string port,emails)
			    with _ -> usage ())
			| server::_ ->
			    (server,25,emails)
			| _ -> usage ())
		  | _ -> usage () in	      
	      Checknode.start
		~smtp_server
		~smtp_port
		emails
	    end	    
	| "test" ->
	   Test.chroots ()
	| "test-changelog" ->
	   let package = Sys.argv.(2) in
	   let version = Sys.argv.(3) in
	   let rev_a = Sys.argv.(4) in
	   let rev_b = Sys.argv.(5) in
	   with_teleport Goto_bf_params
	     (fun () ->
	      ignore (Changelog_ng.make package version (int_of_string rev_a) (int_of_string rev_b)))
	| "test-depload" ->
	   let depfile = Sys.argv.(2) in
	   Test.depload depfile
	| "test-deptree" ->
	   let pkgname = Sys.argv.(2) in
	   let version = Sys.argv.(3) in
	   let revision = int_of_string Sys.argv.(4) in
	   Test.depgraph pkgname version revision
	| _ ->
	    analyze ()
    else usage ()
      
let print_current_state () =
  let param name =
    Printf.printf "%s: %s\n" name (Params.get_param name) in
  Printf.printf "*** CURRENT STATE of BUILDFARM ***\n%!";
  param "git-url";
  param "top-dir";
  param "dev-dir";
  param "dest-dir";
  param "component";
  param "label-type";
  param "label";
  param "start-dir";
  param "pkg-storage";
  param "pkg-branch";
  param "log-level";
  Printf.printf "**********************************\n%!"
   
let string_of_exn = function
  | Unix.Unix_error (error,name,param) ->
      let msg = Unix.error_message error in
      Printf.sprintf "Unix.Unix_error(%s,%s,%s)\n" msg name param
  | exn ->
      Printexc.to_string exn

let _ =
  try main () with
    | exn ->
	let bt = Printexc.get_raw_backtrace () in
	print_current_state ();
	Printf.printf "Exception: %s (main thread)\nBacktrace:\n%s\n%!"
		      (string_of_exn exn) (Printexc.raw_backtrace_to_string bt);
	exit 2




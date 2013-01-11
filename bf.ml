(* Buildfarm for GIT-repository *)

open Types

(* Locking *)

let fd_lock =
  Unix.openfile (Params.get_param "lock-file")
    [Unix.O_CREAT;Unix.O_RDONLY;Unix.O_WRONLY] 0o644
;;

let lock () =
  print_string "locking...";
  Unix.lockf fd_lock Unix.F_LOCK 1;
  print_endline "ok"

let unlock () =
  print_string "unlocking...";
  Unix.lockf fd_lock Unix.F_ULOCK 1;
  Unix.close fd_lock;
  print_endline "ok"

let with_lock f =
  lock (); f (); unlock ()


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
  print_endline "   or: bf fork <specdir> <source-branch> <new-branch> [<single-rev-depth>]";
  print_endline "   or: bf clone <ssh-user>@<ssh-host> <pkg-path> [overwrite|depends|packages]";
  print_endline "   or: bf clone <specdir> [overwrite] [norec] [<ver> <rev>]";
  print_endline "   or: bf link <pkg-path> [symlink]";
  print_endline "   or: bf top <specdir> [overwrite] [norec]";
  print_endline "   or: bf graph <specdir> [<ver> <rev>]";
  print_endline "   or: bf basegraph <specdir> [hard|soft]";
  print_endline "   or: bf tag <composite> <tag>";
  print_endline "   or: bf make (build|install) [<name>]";
  print_endline "   or: bf versions <pkgdir>";
  print_endline "   or: bf search <commit-id>";
  print_endline "   or: bf shell";
  print_endline "   or: bf clean";
  print_endline "   or: bf log";
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
	  Is_components [ make_component two ]
    | 4 ->
	let two = Sys.argv.(2) in
	if Sys.file_exists two && System.is_regular two then
	  Is_composite_with_tag (two,Sys.argv.(3))
	else
	  (match  Sys.argv.(3) with
	    | "branch" | "tag" -> usage ()
	    |  _ -> 
		 Is_components 
		  [ make_component Sys.argv.(2); make_component Sys.argv.(3) ])
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
		}
	  | "tag" ->
	      Is_component_with_label
		{ name = Sys.argv.(2); label = (Tag Sys.argv.(4)); pkg = None; rules = make_rules (); nopack = false }
	  |  _ ->
	       Is_components
		[ 
		  make_component Sys.argv.(2);
		  make_component Sys.argv.(3);
		  make_component Sys.argv.(4);
		])
    | _ ->
	Is_components
	  (List.map make_component (List.tl (List.tl (Array.to_list Sys.argv))))

let main () =
  let len = Array.length Sys.argv in
    if len > 1 then
      let action = Sys.argv.(1) in
      let analyze () =
	match analyze_arguments () with
	  | Is_components components ->
	      (match action with
		| "prepare"   -> Commands.prepare   components
		| "build"     -> Commands.build     components
		| "rebuild"   -> Commands.rebuild   components
		| "install"   -> ignore(Commands.install components)
		| "reinstall" -> Commands.reinstall components
		| "update"    -> ignore(Commands.update components)
		| "forward"   -> Commands.forward   components
		| "status"    -> Commands.status    components
		| _           -> usage ())
	  | Is_component_with_label component ->
	      (match action with
		| "prepare"   -> Commands.prepare   [component]
		| "build"     -> Commands.build     [component]
		| "rebuild"   -> Commands.rebuild   [component]
		| "install"   -> ignore(Commands.install [component])
		| "reinstall" -> Commands.reinstall [component]
		| "update"    -> ignore(Commands.update [component])
		| "forward"   -> Commands.forward   [component]
		| "status"    -> Commands.status    [component]
		| _           -> usage ())
	  | Is_composite composite ->
	      Params.set_composite_mode ();
	      (match action with
		| "prepare"   -> Commands.prepare_composite   composite
		| "build"     -> Commands.build_composite     composite
		| "rebuild"   -> Commands.rebuild_composite   composite
		| "install"   -> ignore(Commands.install_composite composite)
		| "reinstall" -> Commands.reinstall_composite composite
		| "update"    -> ignore(Commands.update_composite composite)
		| "forward"   -> Commands.forward_composite   composite
		| "status"    -> Commands.status_composite    composite
		| _           -> usage ())
	  | Is_composite_with_tag (composite,tag) ->
	      Params.set_composite_mode ();
	      (match action with
		| "prepare"   -> Commands.prepare_composite   ~tag composite
		| "build"     -> Commands.build_composite     ~tag composite
		| "rebuild"   -> Commands.rebuild_composite   ~tag composite
		| "install"   -> ignore(Commands.install_composite  ~tag composite)
		| "reinstall" -> Commands.reinstall_composite ~tag composite
		| "update"    -> ignore(Commands.update_composite ~tag composite)
		| "forward"   -> Commands.forward_composite   ~tag composite
		| "status"    -> Commands.status_composite    ~tag composite
		| _           -> usage ())
      in match action with
	| "pack" ->
	    if len = 5 then
	      let specdir = Sys.argv.(2) in
	      let version = Sys.argv.(3) in
	      let release = Sys.argv.(4) in
	      Pack.build_package
		[specdir;version;release]
	    else usage ()
	| "update" ->
	    if len > 2 then
	      let specdir = Sys.argv.(2) in
	      let version = Filename.concat specdir "version" in
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
		  ignore (Pack.update ~specdir ~lazy_mode ~ver ~rev ()))
	      else
		analyze ()
	    else
	      analyze ()
	| "log" ->
	    if len = 2 then
	      Rules.log_viewer ()
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
		in with_lock (fun () -> Pack.clone ~vr ~recursive ~overwrite Sys.argv.(2))
	      end
	    else
	      begin
		if len = 4 then
		  with_lock (fun () -> Pack.pkg_clone Sys.argv.(2) Sys.argv.(3) "default")
		else
		  if len = 5 then
		    with_lock (fun () -> Pack.pkg_clone Sys.argv.(2) Sys.argv.(3) Sys.argv.(4))
		  else
		    usage ()
	      end
	| "top" ->
	    if Sys.file_exists Sys.argv.(2) then
	      Pack.top Sys.argv.(2)
	    else usage ()
	| "upgrade" ->
	    let (upgrade_mode,default_branch) =
	      match len with
		| 3 -> Upgrade_default, None
		| 4 ->
		    (match Sys.argv.(3) with
		      | "lazy"     -> (Upgrade_lazy,None)
		      | "complete" -> (Upgrade_complete,None)
		      | "full"     -> (Upgrade_full,None)
		      | _          -> (Upgrade_default,Some Sys.argv.(3)))
		| 5 ->
		    (match Sys.argv.(3) with
		      | "lazy"     -> (Upgrade_lazy,Some Sys.argv.(4))
		      | "complete" -> (Upgrade_complete,Some Sys.argv.(4))
		      | "full"     -> (Upgrade_full,Some Sys.argv.(4))
		      | _          ->
			  (match Sys.argv.(4) with
			    | "lazy"     -> (Upgrade_lazy,Some Sys.argv.(3))
			    | "complete" -> (Upgrade_complete,Some Sys.argv.(3))
			    | "full"     -> (Upgrade_full,Some Sys.argv.(3))
			    | _ -> usage ()))
		| _ -> usage ()
	    in
	    with_lock
	      (fun () ->
		Pack.upgrade Sys.argv.(2) upgrade_mode default_branch)
	| "fork" ->
	    if len <> 5 then
	      if len <> 6 then
		usage ()
	      else
		with_lock (fun () ->
		  Pack.fork ~depth:(make_int Sys.argv.(5)) Sys.argv.(2) Sys.argv.(3) Sys.argv.(4))
	    else
	      with_lock (fun () ->
		Pack.fork Sys.argv.(2) Sys.argv.(3) Sys.argv.(4))
	| "graph" ->
	    if len <> 3 then
	      if len <> 5 then
		usage ()
	      else
		with_lock (fun () ->
		  Pack.graph ~ver:Sys.argv.(3) ~rev:(make_int Sys.argv.(4)) Sys.argv.(2))
	    else
	      with_lock (fun () -> Pack.graph Sys.argv.(2))
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
		with_lock (fun () -> Pack.basegraph Sys.argv.(2) (check_mode Sys.argv.(3)))
	    else
	      with_lock (fun () -> Pack.basegraph Sys.argv.(2) "full")
	| "tag" ->
	    if len = 4 then
	      with_lock (fun () ->
		Commands.tag_composite Sys.argv.(2) Sys.argv.(3))
	    else usage ()
	| "review" ->
	    if len = 4 then
	      Commands.review_composite Sys.argv.(2) Sys.argv.(3)
	    else usage ()
	| "diff" ->
	    if len = 5 then
	      if Filename.basename Sys.argv.(2) = "composite" then
		Commands.diff_composite Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	      else
		Pack.diff_packages Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	    else usage ()
	| "changelog" ->
	    if len = 5 || len = 6 then
	      if Filename.basename Sys.argv.(2) = "composite" then
		Commands.changelog_composite ~interactive:true
		  ~compact:(len=6) Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	      else
		Pack.changelog_packages Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	    else usage ()
	| "link" ->
	    if len = 3 then
	      Pack.link ~hard:true Sys.argv.(2)
	    else
	      if len = 4 && Sys.argv.(3) = "symlink" then
		Pack.link ~hard:false Sys.argv.(1)
	      else usage ()
	| "shell" ->
	    Scheme.shell ()
	| "clean" ->
	    Pack.clean ()
	| "make" ->
	    Params.update_param "plugins-dir" "../pack";
	    if len = 3 then
	      (match Sys.argv.(2) with
		| "build" -> Rules.build_rules None
		| "install" -> Rules.install_rules ~check_build:false None
		| _ -> usage ())
	    else if len = 4 then
	      (match Sys.argv.(2) with
		| "build" -> Rules.build_rules (Some Sys.argv.(3))
		| "install" -> Rules.install_rules ~check_build:false (Some Sys.argv.(3))
		| _ -> usage ())
	    else
	      usage ()
	| "versions" ->
	    if len <> 3 then
	      usage ()
	    else
	      with_lock (fun () -> Pack.last_versions Sys.argv.(2))
	| "search" ->
	    if len = 3 then
	      with_lock (fun () ->
		Pack.search Sys.argv.(2))
	    else usage ()	      
	| _ ->
	    analyze ()
    else usage ()
      
let teleport f =
  (* todo: more advanced teleport *)
  if Sys.file_exists ".bf-rules" then (* todo: use alternative rules *)
    begin
      Sys.chdir "..";
      Params.update_param "start-dir"
	((Params.get_param "start-dir") ^ "/..");
      Params.update_param "log-dir"
	((Params.get_param "log-dir") ^ "/..");
      f ()
    end
  else
    f ()

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
  param "log-level";
  Printf.printf "**********************************\n%!"
   
let _ =
  try main () with
    | exn ->
	print_current_state ();
	(match exn with
	  | Logger.Error -> exit 2
	  | Unix.Unix_error (error,name,param) ->
	      let msg = Unix.error_message error in
	      Printf.printf "Fatal error: exception Unix.Unix_error(%s,%s,%s)\n" msg name param;
	      exit 2
	  | exn -> raise exn)
	  

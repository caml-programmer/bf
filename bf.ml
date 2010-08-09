(* Buildfarm for GIT-repository *)

open Types

type analyze_result =
  | Is_components of component list
  | Is_component_with_label of component
  | Is_composite of string
  | Is_composite_with_tag of (string * string)

let usage () =
  print_endline "Usage: bf (prepare|update|forward|[re]build|[re]install|status) <components>";
  print_endline "   or: bf (prepare|update|forward|[re]build|[re]install|status) <component> [branch <branch> | tag <tag>]";
  print_endline "   or: bf (prepare|update|forward|[re]build|[re]install|status) <composite> [<tag>]";
  print_endline "   or: bf (diff|changelog) <composite> <tag-a> <tag-b>";
  print_endline "   or: bf (diff|changelog) <specdir> <rev-a> <rev-b>";
  print_endline "   or: bf review <composite> <since-date>";
  print_endline "   or: bf pack <specdir> <version> <release>";
  print_endline "   or: bf update <specdir> [lazy] [<version>] [<release>]";
  print_endline "   or: bf upgrade <specdir> [lazy|complete] [<branch>]";
  print_endline "   or: bf fork <specdir> <source-branch> <new-branch>";
  print_endline "   or: bf clone <ssh-user>@<ssh-host> <pkg-path> [overwrite|depends|packages]";
  print_endline "   or: bf clone <specdir> [overwrite] [norec] [<ver> <rev>]";
  print_endline "   or: bf top <specdir> [overwrite] [norec]";
  print_endline "   or: bf graph <specdir> [<ver> <rev>]";
  print_endline "   or: bf tag <composite> <tag>";
  print_endline "   or: bf log <logdir>";
  exit 1

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
		{ name = Sys.argv.(2); label = (Branch Sys.argv.(4)); pkg = None }
	  | "tag" ->
	      Is_component_with_label
		{ name = Sys.argv.(2); label = (Tag Sys.argv.(4)); pkg = None }
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
		| "install"   -> Commands.install   components
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
		| "install"   -> Commands.install   [component]
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
		| "install"   -> Commands.install_composite   composite
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
		| "install"   -> Commands.install_composite   ~tag composite
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
		ignore (Pack.update ~specdir ~lazy_mode ~ver ~rev ())
	      else
		analyze ()
	    else
	      analyze ()
	| "log" ->
	    if len = 3 then
	      Rules.log_wizor Sys.argv.(2)
	    else usage ()
	| "clone" ->
	    if Sys.file_exists Sys.argv.(2) then
	      begin
		let check_rec s = if s = "norec" then false else usage () in
		let check_over s = if s = "overwrite" then true else usage () in
		let make_int s =
		  try int_of_string s with _ -> usage () in
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
		  else if len = 7 then
		    (match Sys.argv.(3) with
		      | "overwrite" -> (check_rec
			  Sys.argv.(4),true,Some (Sys.argv.(5), make_int Sys.argv.(6)))
		      | "norec" -> (false,check_over Sys.argv.(4),Some (Sys.argv.(5), make_int Sys.argv.(6)))
		      | _ -> usage ())
		  else
		    usage ()
		in Pack.clone ~vr ~recursive ~overwrite Sys.argv.(2)
	      end
	    else
	      begin
		if len = 4 then
		  Pack.pkg_clone Sys.argv.(2) Sys.argv.(3) "default"
		else
		  if len = 5 then
		    Pack.pkg_clone Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
		  else
		    usage ()
	      end
	| "top" ->
	    if Sys.file_exists Sys.argv.(2) then
	      begin
		let check_rec s = if s = "norec" then false else usage () in
		let check_over s = if s = "overwrite" then true else usage () in
		let (recursive,overwrite) =
		  if len = 3 then
		    (true,false)
		  else if len = 4 then
		    (match Sys.argv.(3) with
		      | "overwrite" -> (true,true)
		      | "norec" -> (false,false)
		      | _ -> usage ())
		  else if len = 5 then
		    (match Sys.argv.(3) with
		      | "overwrite" -> (check_rec Sys.argv.(4),true)
		      | "norec" -> (false,check_over Sys.argv.(4))
		      | _ -> usage ())
		  else usage ()
		in Pack.top ~recursive ~overwrite Sys.argv.(2)
	      end
	    else usage ()
	| "upgrade" ->
	    let (upgrade_mode,default_branch) =
	      match len with
		| 3 -> Upgrade_full, None
		| 4 ->
		    (match Sys.argv.(3) with
		      | "lazy"     -> (Upgrade_lazy,None)
		      | "complete" -> (Upgrade_complete,None)
		      | _          -> (Upgrade_full,Some Sys.argv.(3)))
		| 5 ->
		    (match Sys.argv.(3) with
		      | "lazy"     -> (Upgrade_lazy,Some Sys.argv.(4))
		      | "complete" -> (Upgrade_complete,Some Sys.argv.(4))
		      | _          ->
			  (match Sys.argv.(4) with
			    | "lazy"     -> (Upgrade_lazy,Some Sys.argv.(3))
			    | "complete" -> (Upgrade_complete,Some Sys.argv.(3))
			    | _ -> usage ()))
		| _ -> usage ()
	    in Pack.upgrade Sys.argv.(2) upgrade_mode default_branch
	| "fork" ->
	    if len <> 5 then
	      usage ()
	    else Pack.fork Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	| "graph" ->
	    if len <> 3 then
	      if len <> 5 then
		usage ()
	      else
		Pack.graph ~ver:Sys.argv.(3) ~rev:Sys.argv.(4) Sys.argv.(2)
	    else
		Pack.graph Sys.argv.(2)
	| "tag" ->
	    if len = 4 then
	      Commands.tag_composite Sys.argv.(2) Sys.argv.(3)
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
	    if len = 5 then
	      if Filename.basename Sys.argv.(2) = "composite" then
		Commands.changelog_composite Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	      else
		Pack.changelog_packages Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	    else usage ()
	| _ ->
	    analyze ()
    else usage ()
      
let teleport f =
  (* todo: more advanced teleport *)
  if Sys.file_exists ".bf-rules" then
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

let _ =
  let current = Sys.getcwd () in
  try    
    main ()
  with exn ->
    Sys.chdir current; raise exn

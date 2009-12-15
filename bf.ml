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
  print_endline "   or: bf (prepare|update|forward|[re]build|[re]install|status) <composite> [tag]";
  print_endline "   or: bf (diff|changelog) <composite> <tag-a> <tag-b>";
  print_endline "   or: bf review <composite> <since-date>";
  print_endline "   or: bf pack <specdir> <version> <release>";
  print_endline "   or: bf update <specdir> [version] [release]";
  print_endline "   or: bf upgrade <specdir> [lazy]";
  print_endline "   or: bf clone <ssh-user>@<ssh-host> <pkg-path> [overwrite|depends]";
  print_endline "   or: bf tag <composite> <tag>";
  print_endline "   or: bf log <logdir>";
  exit 1

let make_component s =
  { name = s; label = Current; pkg = None }

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
	      let ver =
		if len > 3 then
		  Some Sys.argv.(3)
		else None
	      in
	      let rev =
		if len > 4 then
		  Some Sys.argv.(4)
		else None
	      in
	      if Sys.file_exists version then
		Pack.update ~specdir ~ver ~rev ()
	      else
		analyze ()
	    else
	      analyze ()
	| "log" ->
	    if len = 3 then
	      Rules.log_wizor Sys.argv.(2)
	    else usage ()
	| "clone" ->
	    if len = 4 then
	      Pack.clone Sys.argv.(2) Sys.argv.(3) "default"
	    else
	      if len = 5 then
		Pack.clone Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	      else
		usage ()
	| "upgrade" ->
	    if len = 3 then
	      Pack.upgrade Sys.argv.(2) false
	    else 
	      if len = 4 then
		Pack.upgrade Sys.argv.(2) true
	      else
		usage ()
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
	      Commands.diff_composite Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
	    else usage ()
	| "changelog" ->
	    if len = 5 then
	      Commands.changelog_composite Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
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

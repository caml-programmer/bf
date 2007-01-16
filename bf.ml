(* Buildfarm for GIT-repository *)

open Types

type analyze_result =
  | Is_components of component list
  | Is_component_with_label of component
  | Is_composite of string

let usage () =
  print_endline "Usage: bf (prepare|update|forward|[re]build|[re]install) <components>";
  print_endline "   or: bf (prepare|update|forward|[re]build|[re]install) <component> [branch <branch> | tag <tag>]";
  print_endline "   or: bf (prepare|update|forward|[re]build|[re]install) <composite>";
  exit 1

let make_component s =
  { name = s; label = Current }

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
	(match  Sys.argv.(3) with
	  | "branch" | "tag" -> usage ()
	  |  _ -> 
	       Is_components 
		[ make_component Sys.argv.(2); make_component Sys.argv.(3) ])
    | 5 ->
	(match  Sys.argv.(3) with
	  | "branch" ->
	      Is_component_with_label
		{ name = Sys.argv.(2); label = (Branch Sys.argv.(4))}
	  | "tag" ->
	      Is_component_with_label
		{ name = Sys.argv.(2); label = (Tag Sys.argv.(4))}
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
    if Array.length Sys.argv > 1 then
      let action = Sys.argv.(1) in
      match analyze_arguments () with
	| Is_components components ->
	    (match action with
	      | "prepare"   -> Commands.prepare   components
	      | "build"     -> Commands.build     components
	      | "rebuild"   -> Commands.rebuild   components
	      | "install"   -> Commands.install   components
	      | "reinstall" -> Commands.reinstall components
	      | "update"    -> Commands.update    components
	      | "forward"   -> Commands.forward   components
	      | _           -> usage ())
	| Is_component_with_label component ->
	    (match action with
	      | "prepare"   -> Commands.prepare   [component]
	      | "build"     -> Commands.build     [component]
	      | "rebuild"   -> Commands.rebuild   [component]
	      | "install"   -> Commands.install   [component]
	      | "reinstall" -> Commands.reinstall [component]
	      | "update"    -> Commands.update    [component]
	      | "forward"   -> Commands.forward   [component]
	      | _           -> usage ())
	| Is_composite composite ->
	    Params.set_composite_mode ();
	    (match action with
	      | "prepare"   -> Commands.prepare_composite   composite
	      | "build"     -> Commands.build_composite     composite
	      | "rebuild"   -> Commands.rebuild_composite   composite
	      | "install"   -> Commands.install_composite   composite
	      | "reinstall" -> Commands.reinstall_composite composite
	      | "update"    -> Commands.update_composite    composite
	      | "forward"   -> Commands.forward_composite   composite
	      | _           -> usage ())
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

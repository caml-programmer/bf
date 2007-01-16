(* Buildfarm for GIT-repository *)

open Types

let usage () =
  print_endline "Usage: bf (prepare|build|rebuild|install) <components>";
  print_endline "   or: bf (prepare|build|rebuild|install) <component> [branch <branch> | tag <tag>]";
  print_endline "   or: bf (prepare|build|rebuild|install) <composite>";
  exit 1

let make_component s =
  { name = s; label = Current }

let _ =
  let len = Array.length Sys.argv in
  if len > 1 then
    let components =
      List.map make_component (List.tl (List.tl (Array.to_list Sys.argv))) in
      match Sys.argv.(1) with
	| "prepare" -> Commands.prepare components
	| "build"   -> Commands.build   components
	| "rebuild" -> Commands.rebuild components
	| "install" -> Commands.install components
	| _ -> usage ()
  else usage ()
    

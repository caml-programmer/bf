(* Buildfarm for GIT-repository *)

let usage () =
  print_endline "Usage: bf (prepare|build|rebuild|install) components";
  exit 1

let _ =
  let len = Array.length Sys.argv in
  if len > 1 then
    let components =
      List.tl (List.tl (Array.to_list Sys.argv)) in
      match Sys.argv.(1) with
	| "prepare" -> Commands.prepare components
	| "build"   -> Commands.build   components
	| "rebuild" -> Commands.rebuild components
	| "install" -> Commands.install components
	| _ -> usage ()
  else usage ()
    

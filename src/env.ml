(** Component envirionment support *)

let system_environment = Hashtbl.create 32;;
let component_environment = Hashtbl.create 32;;

let init_system_environment () =
  Hashtbl.clear system_environment;
  Array.iter
    (fun s ->
      (try
	let (key,value) = System.split_env_var s in
	Hashtbl.add system_environment key value
      with Not_found -> ()))
    (Unix.environment ())

let build_component_environment () =
  Hashtbl.iter
    (fun k v ->      
      if not (Hashtbl.mem component_environment k) then
	Hashtbl.add component_environment k v)
    system_environment
    
let view env =
  let make key value acc =
    acc @ [key ^ "=" ^ value] in 
  Array.of_list
    (Hashtbl.fold make env [])
  
let system () =
  view system_environment

let component () =
  build_component_environment ();
  view component_environment

let prepare () =
  Hashtbl.clear component_environment;
  build_component_environment ()
  
let update n v =
  Hashtbl.replace component_environment n v

let get n =
  Hashtbl.find component_environment n

let find_system n =
  Hashtbl.find system_environment n

let find_component n =
  Hashtbl.find component_environment n

;;

init_system_environment ();;



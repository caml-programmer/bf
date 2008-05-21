(** Component envirionment support *)

let system_environment = Hashtbl.create 32;;
let component_environment = Hashtbl.create 32;;

let set_current () =
  Hashtbl.clear system_environment;
  Array.iter
    (fun s ->
      (try
	let (key,value) = System.split_env_var s in
	Hashtbl.add system_environment key value
      with Not_found -> ()))
    (Unix.environment ());
  Hashtbl.iter
    (Hashtbl.replace system_environment)
    component_environment
    
let system_env_view env =
  let view key value acc =
    acc @ [key ^ "=" ^ value]
  in Array.of_list
       (Hashtbl.fold view env [])
  
let current () =
  set_current ();
  system_env_view
    system_environment
  
let prepare () =
  Hashtbl.clear component_environment
  
let update n v =
  Hashtbl.replace component_environment n v

let find n =
  set_current ();
  Hashtbl.find
    system_environment n




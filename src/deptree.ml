(* Depend tree support *)

exception Tree_error of string

type 'a deptree =
  | Dep_val of 'a * 'a deptree
  | Dep_list of 'a deptree list

type 'a graph = ('a * 'a list) list

exception Not_found_vertex

let create_graph () = []

let insert_vtx g v =
  if List.mem_assoc v g then g
  else (v,[])::g

let insert_edge g a b =
  if not (List.mem_assoc a g) then
    raise Not_found_vertex;
  if not (List.mem_assoc b g) then
    raise Not_found_vertex;
  List.map 
    (fun (k,l) ->
      if k = a then
	if List.mem b l then
	  (k,l)
	else
	  (k,b::l)
      else
	(k,l)) g

let remove_vtx g v =
  if List.mem_assoc v g then
    List.map (fun (k,vl) -> k,(List.filter (fun x -> x <> v) vl))
      (List.filter (fun (k,_) -> k <> v) g)
  else g

let has_edges_to g v =
  if List.mem_assoc v g then
    List.assoc v g
  else raise Not_found_vertex

let has_edges_from g v =
  if List.mem_assoc v g then
    begin
      List.fold_left 
	(fun acc (k,vl) ->
	  if List.mem v vl then
	    k::acc
	  else acc) [] g
    end
  else raise Not_found_vertex
  
let find_finish_vtx g =
  List.map fst (List.filter (fun (k,vl) -> vl = []) g)
    
exception Cannot_unwind_depends_graph

let unwind g =
  let acc = ref [] in
  let work = ref g in
  let remove g v =
    acc := v :: !acc;
    work := remove_vtx !work v;
  in
  let counter = ref 0 in
  while !work <> [] do
    incr counter;
    if !counter > 1000000000 then
      raise Cannot_unwind_depends_graph
    else
      List.iter (remove !work)
	(find_finish_vtx !work)
  done;
  (List.rev !acc)

let deplist_of_deptree tree =
  let rec make depth = function
    | Dep_val (x, Dep_list l) -> (List.flatten (List.map (make (succ depth)) l)) @ [x,depth]
    | _ -> assert false
  in make 0 tree

let list_of_deptree tree =
  let g = ref (create_graph ()) in
  let rec fill_graph parent = function
    | Dep_val (x, Dep_list l) ->
	(match parent with
	  | Some p ->
	      g := insert_vtx !g p;
	      g := insert_vtx !g x;
	      g := insert_edge !g p x;
	  | None -> ());
	List.iter (fill_graph (Some x)) l
    | _ -> assert false
  in fill_graph None tree;
  unwind !g

let rec map_deptree f = function
  | Dep_val (x, tree) ->
      Dep_val (f x, map_deptree f tree)
  | Dep_list l ->
      Dep_list (List.map (map_deptree f) l)

open Ext

type 'a digraph = ('a * 'a list) list
type 'a t = 'a digraph
				 
exception Vertex_already_exists
exception Vertex_not_found
exception Head_not_found
exception Tail_not_found
exception Edge_already_exists
exception Edge_not_found
exception Circular_path
exception Path_not_found
exception Circular_path_not_found
	    
let create () = []

let has_vertex g v = Alist.has_key v g

let vertices = Alist.keys

let edges graph =
  List.flatten
    (List.map (fun (vertex, tails) ->
	       List.map (fun tail -> (vertex, tail)) tails)
	      graph)
		 
let insert_vertex g v =
  if has_vertex g v then
    raise Vertex_already_exists
  else Alist.add v [] g

let insert_edge graph newhead newtail =
  if not (has_vertex graph newhead) then raise Head_not_found;
  if not (has_vertex graph newtail) then raise Tail_not_found;
  List.map (fun (head, tails) ->
	    if head = newhead then
	      if List.mem newtail tails then
		raise Edge_already_exists
	      else (head, newtail::tails)
	    else (head, tails))
	   graph

let not_vertex v1 v2 = v1 <> v2
	   
let remove_vertex g v =
  if has_vertex g v then
    List.map (fun (head, tails) ->
	      (head, (List.filter (not_vertex v) tails)))
	     (Alist.remove v g)
  else raise Vertex_not_found

let remove_edge graph h t =
  if not (has_vertex graph h) then raise Head_not_found;
  if not (has_vertex graph t) then raise Tail_not_found;
  List.map (fun (head, tails) ->
	    if head = h then
	      if List.mem t tails then
		(head, (List.filter (not_vertex t) tails))
	      else raise Edge_not_found
	    else (head, tails))
	   graph

let get_tails g v = Alist.assoc v g
let get_vertex g v = v,(get_tails g v)

let find_path ?(ignore_start=false) (graph: 'a digraph) start destination : 'a list =
  let rec find path tails =
    if List.mem destination tails then
      List.rev (destination :: path)
    else
      match tails with
      | [] -> raise Path_not_found
      | v :: r -> (if List.mem v path then raise Circular_path;
		   try find (v::path) (get_tails graph v) with
		   | Path_not_found
		   | Circular_path ->
		      find path r) in
  match ((start=destination),ignore_start) with
  | (true,false) -> [start]
  | _ -> find [start] (get_tails graph start)


let find_circular_path graph =
  let rec check = function
    | [] -> raise Circular_path_not_found
    | v :: rest_vertices ->
       try find_path ~ignore_start:true graph v v with
         Path_not_found -> check rest_vertices in
  check (vertices graph)

let has_circular_path graph =
  try ignore (find_circular_path graph); true
  with Circular_path_not_found -> false

let count_input_edges graph vertex =
  List.length
    (List.filter (fun (head,tail) -> tail = vertex)
		 (edges graph))

let count_output_edges graph vertex =
  List.length
    (List.filter (fun (head,tail) -> head = vertex)
		 (edges graph))

exception Root_not_exists
exception Multiple_roots_exist
exception Leaves_not_found
    
let find_root graph =
  let roots = List.filter (fun vertex -> (count_input_edges graph vertex) = 0)
			  (vertices graph) in
  match roots with
  | [root] -> root
  | [] -> raise Root_not_exists
  | _ -> raise Multiple_roots_exist

let find_leaves graph =
  let leaves = List.filter (fun vertex -> (count_output_edges graph vertex) = 0)
			   (vertices graph) in
  match leaves with
  | [] -> raise Leaves_not_found
  | _ -> leaves

let rec union = function
  | [] -> []
  | g1 :: [] -> g1
  | g1 :: g2 :: graphs ->
     let vertices = List.remove_duplicates ((vertices g1) @ (vertices g2)) in
     let edges = List.remove_duplicates ((edges g1) @ (edges g2)) in
     let (heads,tails) = List.split edges in
     let g = create () in
     let g = List.fold_left insert_vertex g vertices in
     let g = List.fold_left2 insert_edge g heads tails in
     union (g::graphs)
	   
(*
let _ =
  let g = create () in

  let g = insert_vertex g "a" in
  let g = insert_vertex g "b" in
  let g = insert_vertex g "c" in
  let g = insert_vertex g "d" in
  let g = insert_vertex g "e" in

  let g = insert_edge g "a" "b" in
  let g = insert_edge g "b" "c" in
  let g = insert_edge g "c" "d" in
  let g = insert_edge g "d" "e" in
  let g = insert_edge g "e" "b" in

  List.iter (Printf.printf "%s ") (find_circular_path g);
  (*(find_path ~ignore_start:true g "a" "a")*)
  print_endline ""
 *)
	


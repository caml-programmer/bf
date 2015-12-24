(* нагруженный ориентированный граф *)
(* loaded directed grapg*)

(* сокращения:
 * g - graph
 * h - head
 * t - tail
 * w - weight
 *)

open Ext

exception Vertex_already_exists
exception Vertex_not_found
exception Head_not_found
exception Tail_not_found
exception Edge_already_exists
exception Edge_not_found
exception Circular_path
exception Path_not_found
exception Circular_path_not_found
exception Weight_not_found
	    
type ('a,'b) ldigraph =
  {
    mutable vertices: 'a list;
    mutable edges: ('a * 'a) list;
    weights: (('a*'a),'b) Hashtbl.t;
  }

type ('a,'b) t = ('a,'b) ldigraph			  

let create () =
  {
    vertices = [];
    edges = [];
    weights = Hashtbl.create 0;
  }

let vertices g = g.vertices
let edges g = g.edges
let weights g = g.weights

let copy g =
  {
    vertices = vertices g;
    edges = edges g;
    weights = Hashtbl.copy (weights g)
  }
		  
let has_vertex g v =  List.mem v g.vertices
let has_edge g e = List.mem e g.edges
let has_weight g e = Hashtbl.mem g.weights e
	   
let insert_vertex g v =
  g.vertices <- v :: g.vertices

let insert_edge g (h,t) =
  if not (has_vertex g h) then raise Head_not_found;
  if not (has_vertex g t) then raise Tail_not_found;
  g.edges <- (h,t) :: g.edges

let set_weight g e w =
  if not (has_edge g e) then raise Edge_not_found;
  let set = if Hashtbl.mem g.weights e
	    then Hashtbl.replace g.weights e
	    else Hashtbl.add g.weights e in
  set w

let weight g e = Hashtbl.find g.weights e
      
let vertex_edges g v =
  List.filter (fun (h,t) -> (h = v) || (t = v)) (edges g)

let in_edges g v =
  List.filter (fun (h,t) -> t = v) (edges g)

let out_edges g v =
  List.filter (fun (h,t) -> h = v) (edges g)

let in_weights g v =
  let edges = in_edges g v in
  List.map (weight g) edges
	      
let count_in_edges g v =
  List.length (in_edges g v)

let count_out_edges g v =
  List.length (out_edges g v)

let roots g =
  List.filter (fun v -> (count_in_edges g v) = 0)
	      (vertices g)
	      
let leaves g =
  List.filter (fun v -> (count_out_edges g v) = 0)
	      (vertices g)

let unset_weight g e =
  Hashtbl.remove g.weights e

let del_edge g e =
  unset_weight g e;
  g.edges <- List.remove_elt e g.edges
	      
let del_vertex g v =
  let edges = vertex_edges g v in
  g.vertices <- List.remove_elt v g.vertices;
  List.iter (del_edge g) edges

let edge_head (h,t) = h
	    
let edge_tail (h,t) = t
	    
let find_path ?(ignore_start=false) (g: ('a,'b) ldigraph) start fin : 'a list =
  let get_tails v = List.map edge_tail (out_edges g v) in
  let rec find path tails =
    if List.mem fin tails then
      List.rev (fin::path)
    else
      match tails with
      | [] -> raise Path_not_found
      | v :: r ->
	 if List.mem v path then raise Circular_path;
	 try find (v::path) (get_tails v)
	 with Path_not_found | Circular_path -> find path r in
  if (start = fin) && (not ignore_start) then
    if has_vertex g start then
      [start]
    else raise Vertex_not_found
  else
    find [start] (get_tails start)

let find_circular_path g =
  let rec check = function
    | [] -> raise Not_found
    | v :: r ->
       try find_path ~ignore_start:true g v v
       with Path_not_found -> check r in
  check (vertices g)

let has_circular_path g =
  try ignore (find_circular_path g); true
  with Not_found -> false

let rec union = function
  | [] -> raise (Invalid_argument "[]")
  | graphs ->
     {
       vertices = List.remove_duplicates (List.flatten (List.map vertices graphs));
       edges = List.remove_duplicates (List.flatten (List.map edges graphs));
       weights = Hashtbl.union (List.map weights graphs);
     }
				  

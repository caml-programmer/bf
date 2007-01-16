(* Scheme execution *)

open Ocs_prim
open Ocs_types
open Types

let env = Ocs_top.make_env ();;
let thread = Ocs_top.make_thread ();;

let output_scheme_value ch v =
  let port = Ocs_port.output_port ch in
  Ocs_print.print port false v;
  Ocs_port.flush port

let error sval =
  print_endline "Error: invalid scheme value:\n";
  output_scheme_value stdout sval; exit 4

let string_of_sval = function
  | Sstring s -> s
  | Ssymbol s -> s
  | sval      -> error sval

let string_list_of_sval_array v =
  List.map string_of_sval (Array.to_list v)

let list_of_sval v =
  let rec make acc = function      
    | Snull -> acc
    | Spair p ->
	make (acc@[p.car]) p.cdr
    | sval -> error sval
  in make [] v

let eval_file file =
  Ocs_prim.load_file env thread file

let eval_code handler s =
  Ocs_eval.eval thread handler
    (Ocs_compile.compile env (Ocs_read.read_from_string s))

let eval_sval s =
  match s with
    | Sstring x -> Sstring x
    | _ ->
	begin
	  let res = ref None in
	  let handler sval =
	    res := Some sval in
	  Ocs_eval.eval thread handler
	    (Ocs_compile.compile env s);
	  match !res with
	    | None -> error s
	    | Some sval -> sval
	end

let component_of_sval s =
  match s with
    | Spair v ->
	let name =
	  (match v.car with
	    | Ssymbol s -> s
	    | sval -> error sval)
	in
	let label =
	  (match v.cdr with
	    | Snull -> Current
	    | Spair v2 ->
		(match v2.car with
		  | Spair v3 ->
		      (match v3.car with 
			| Ssymbol "branch" ->
			    Branch
			      (match v3.cdr with
				| Spair v4 ->
				    (match v4.car with
					Sstring s -> s
				      | sval -> error sval)
				| sval -> error sval)
			| Ssymbol "tag" ->
			    Tag
			      (match v3.cdr with
				| Spair v4 ->
				    (match v4.car with
					Sstring s -> s
				      | sval -> error sval)
				| sval -> error sval)
			| sval -> error sval)
		  | sval -> error sval)
	    | sval -> error sval)
	in { name = name ; label = label }
    | sval -> error sval
 
let components_of_sval_array v =
  List.map component_of_sval (Array.to_list v)

let eval_pair v =
  let (a,b) = v in
  (match a with
    | Ssymbol s -> s
    | Sstring s -> s
    | sval -> error sval),
  (match b with
    | Sstring s -> s
    | Ssymbol s -> s
    | _ ->
	(match (eval_sval b) with
	  | Ssymbol s -> s
	  | Sstring s -> s
	  | sval -> error sval))

let eval_pair_opt v =
  let (a,b) = v in
  (* error b;*)
  (match a with
    | Ssymbol s -> s
    | Sstring s -> s
    | sval -> error sval),
  (match b with
    | Sstring s -> Some s
    | Ssymbol s -> Some s
    | Snull -> None
    | _ ->
	(match (eval_sval b) with
	  | Ssymbol s -> Some s
	  | Sstring s -> Some s
	  | Snull     -> None
	  | sval -> error sval))

let make_pair f = function
  | Spair v ->
      (match v.cdr with
	| Spair x ->
	    (match x.car with
	      | Sstring s ->
		  f (v.car,Sstring s)
	      | Ssymbol n as sym ->
		  (* Global variables handling *)
		  (match Ocs_env.find_var env sym with
		    | Some (Vglob g) ->
			(match g.g_val with
			  | Sstring s as gs -> f (v.car,gs)
			  | _               -> f (v.car,sym))
		    | _  ->  f (v.car,sym))
	      | sval -> f (v.car,sval))
	| Snull ->
	    f (v.car,Snull)
	| sval -> error sval)
  | sval -> error sval

let rec make_list acc = function
  | Snull -> acc
  | Spair v ->
      make_list (acc@[v.car]) v.cdr
  | sval -> error sval

let env_list_of_sval v =
  List.map (make_pair eval_pair) (make_list [] v)

let make_params_of_sval v =
  (* error v; *)
  List.map (make_pair eval_pair_opt) (make_list [] v)

let unit_handler_of_sval v =
  (fun () -> 
    match v with
	Sproc (p,e) ->
	  let res = ref None in
	  let handler sval =
	    res := Some sval in
	  Ocs_eval.eval thread handler p.proc_body;
	  (match !res with
	    | None -> error v
	    | Some sval -> sval)
      | sval -> error sval)

let string_handler_of_sval v =
  (fun file ->
    match v with
	Sproc (p,e) ->
	  let res = ref None in
	  let handler sval =
	    res := Some sval in	  		  
	  Ocs_eval.eval thread handler
	    (Capply1 ((Cval (Sproc (p,e))),Cval (Sstring file)));	  	  
	  (match !res with
	    | None -> error v
	    | Some sval -> ())
      | sval -> error sval)
  








(* Scheme execution *)

open Ocs_prim
open Ocs_types
open Types

let env = Ocs_top.make_env ();;
let thread = Ocs_top.make_thread ();;

exception Invalid_scheme_value of sval

let string_of_sval = function
  | Sstring s -> s
  | Ssymbol s -> s
  | sval      -> raise (Invalid_scheme_value sval)

let string_list_of_sval_array v =
  List.map string_of_sval (Array.to_list v)

let eval_file file =
  Ocs_prim.load_file env thread file

let eval_code handler s =
  Ocs_eval.eval thread handler
    (Ocs_compile.compile env (Ocs_read.read_from_string s))

let output_scheme_value ch v =
  let port = Ocs_port.output_port ch in
  Ocs_print.print port false v;
  Ocs_port.flush port

let component_of_sval s =
  let error sval =
    print_endline "Error: invalid scheme value:\n";
    output_scheme_value stdout sval; exit 4 in
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
	    | Spair v ->
		(match v.car with
		  | Ssymbol label_type -> 
		      (match label_type with
			| "branch" ->
			    (match v.cdr with
			      | Sstring s -> Branch s
			      | sval -> error sval)
			| "tag" ->
			    (match v.cdr with
			      | Sstring s -> Tag s
			      | sval -> error sval)
			| _ -> error (Spair v))
		  | sval -> error sval)
	    | sval -> error sval)
	in { name = name ; label = label }
    | sval -> error sval
 
let components_of_sval_array v =
  List.map component_of_sval (Array.to_list v)
  

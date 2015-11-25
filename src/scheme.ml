(* Scheme execution *)

open Ocs_prim
open Ocs_types
       
let env = Ocs_top.make_env ();;
let thread = Ocs_top.make_thread ();;

let output_scheme_value ch v =
  let port = Ocs_port.output_port ch in
  Ocs_print.print port false v;
  Ocs_port.flush port

let print v =
  let p = Ocs_port.output_port stdout in
  Ocs_print.print p false v;
  Ocs_port.flush p;
  print_newline ()

let error sval =
  print_endline "Error: invalid scheme value:\n";
  output_scheme_value stdout sval; exit 4

let string_of_sval = function
  | Sstring s -> s
  | Ssymbol s -> s
  | sval      -> error sval

let write_scheme_value fn v =
  let port = Ocs_port.open_output_port fn in
  Ocs_print.print port false v;
  Ocs_port.flush port;
  Ocs_port.close port

let string_list_of_sval_array v =
  List.map string_of_sval (Array.to_list v)

let list_of_sval v =
  let rec make acc = function
    | Snull -> List.rev acc
    | Spair p ->
	make (p.car::acc) p.cdr
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

let shell () =
  while true do
    try
      print_string "ocs> "; flush stdout;
      (Ocs_eval.eval thread print
	(Ocs_compile.compile env 
	  (Ocs_read.read_from_port 
	    (Ocs_port.input_port stdin))))
    with exn ->
      Printf.eprintf "error> %s\n%!" (Printexc.to_string exn)
  done

let defined name =
  match Ocs_vartable.var_find env.env_vartable name with
    | Some _ -> true
    | None -> false
	  
let fst = function
  | Spair v -> v.car
  | x -> error x

let snd = function
  | Spair v -> v.cdr
  | x -> error x

let first = fst

let second = function
  | Spair v -> fst v.cdr
  | x -> error x
      
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
  | Snull -> List.rev acc
  | Spair v ->
      make_list (v.car::acc) v.cdr
  | sval -> error sval

let env_list_of_sval v =
  List.map (make_pair eval_pair) (make_list [] v)

let make_params_of_sval v =
  (* error v; *)
  List.map (make_pair eval_pair_opt) (make_list [] v)

let unit_handler_of_sval v =
  (fun () -> 
    match v with
	Sproc (p,disp) ->
	  let res = ref None in
	  let handler sval =
	    res := Some sval in
	  let th = {
	    thread with
	      th_frame = Array.make p.proc_frame_size Seof;
	      th_display = disp;
	      th_depth = Array.length disp }
	  in
	  Ocs_eval.eval th handler p.proc_body;
	  (match !res with
	    | None -> error v
	    | Some sval -> sval)
      | sval -> error sval)

let string_handler_of_sval v =
  (fun file ->
    match v with
	Sproc (p,disp) ->
	  let res = ref None in
	  let handler sval =
	    res := Some sval in
	  let th = {
	    thread with
	      th_frame = Array.make p.proc_frame_size Seof;
	      th_display = disp;
	      th_depth = Array.length disp }
	  in	  
	  Ocs_eval.eval th handler
	    (Capply1 ((Cval (Sproc (p,disp))),Cval (Sstring file)));
	  (match !res with
	    | None -> error v
	    | Some sval -> ())
      | sval -> error sval)

let unpair = function
  | Spair v -> v
  | _ -> raise Not_found

exception Not_found_string
	       
let make_string = function
  | Sstring s -> s
  | Ssymbol s -> s
  | _ -> raise Not_found_string

let make_int = function
  | Sint n -> n
  | Sstring s -> int_of_string s
  | _ -> raise Not_found

let make_bool = function
  | Strue -> true
  | Sfalse -> false
  | _ -> raise Not_found

let rec iter f = function
  | Spair x -> f x.car; iter f x.cdr
  | _ -> ()

let rec map f = function
  | Spair x ->
      let r = f x.car in
      r :: (map f x.cdr)
  | _ -> []

let read_list =
  map (fun v -> v)
	   
exception Key_not_found of string
exception Not_alist
exception Not_list
	    
let rec filter pred = function
  | Spair x ->
     if pred x.car
     then x.car :: (filter pred x.cdr)
     else filter pred x.cdr
  | Snull -> []
  | _ -> raise Not_list

let filter_key key alist =
  filter (function
	   | Spair pair -> (make_string pair.car) = key
	   | _ -> raise Not_alist)
	 alist

let rec assoc key = function
  | Spair x ->
     if key = (make_string x.car)
     then x.cdr
     else assoc key x.cdr
  | Snull -> raise (Key_not_found key)
  | _ -> raise Not_alist

let condrun name f = function
  | Spair x ->
      if x.car = Ssymbol name then
	f x.cdr
  | _ -> ()

let rec parse m = function
  | Spair x ->
      (match x.car with
	| Ssymbol s ->
	    (try
	      (List.assoc s m) x.cdr
	    with Not_found -> ())
	| Spair _ as y -> parse m y
	| _ -> ());
      parse m x.cdr
  | _ -> ()


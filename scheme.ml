(* Scheme execution *)

open Ocs_prim
open Ocs_types

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
  Printf.printf "eval_file %s\n" file;
  Ocs_prim.load_file env thread file

let eval_code handler s =
  Printf.printf "eval_code %s\n" s;
  Ocs_eval.eval thread handler
    (Ocs_compile.compile env (Ocs_read.read_from_string s))

let output_scheme_value ch v =
  let port = Ocs_port.output_port ch in
  Ocs_print.print port false v;
  Ocs_port.flush port
 

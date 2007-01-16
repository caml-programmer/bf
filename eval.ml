exception Eval_error of string

let eval_reset () =
  Toploop.initialize_toplevel_env ()

let eval_phrase s =
  try
    ignore
      (Toploop.execute_phrase false Format.std_formatter 
	(!Toploop.parse_toplevel_phrase (Lexing.from_string s)))
  with exn -> raise (Eval_error (Printexc.to_string exn))
    
let eval_value v =
  Obj.obj (Toploop.getvalue v)

let eval v exp =
  eval_phrase exp; eval_value v

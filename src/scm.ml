open Ocs_types
open Ocs_sym
open Ocs_numstr

let putc = Buffer.add_char
let puts = Buffer.add_string

let depth = ref 0
let down () = incr depth
let up () = decr depth
let step () = String.make !depth ' '
let write_step p = puts p (step ())
let sublist_flag = ref true
let set_sublist_flag () = sublist_flag := true
let drop_sublist_flag () = sublist_flag := false

let write_string p s =
  putc p '\"';
  for i = 0 to String.length s - 1 do
    match s.[i] with
	'\n' -> puts p "\\n"
      | '\r' -> puts p "\\r"
      | '\t' -> puts p "\\t"
      | '\\' -> puts p "\\\\"
      | '\"' -> puts p "\\\""
      | '\032' .. '\126' as c -> putc p c
      | c -> putc p c
	  (* puts p (Printf.sprintf "\\x%02x" (int_of_char c)) *)
  done;
  putc p '\"'

let escape_string s =
  let b = Buffer.create 32 in
  write_string b s;
  Buffer.contents b
  
let write_char p c =
  puts p "#\\";
  match c with
      '\033' .. '\126' -> putc p c
    | c -> puts p (Ocs_char.char_to_name c)

let rec write_vector p disp v =
  puts p "#(";
  for i = 0 to Array.length v - 1 do
    if i <> 0 then putc p ' ';
    print p disp v.(i)
  done;
  putc p ')'

and write_list p disp l =
  if !depth > 0 then
    begin
      if not !sublist_flag then
	begin
	  putc p '\n';
	  write_step p;
	end
    end;
  down ();
  set_sublist_flag ();
  putc p '(';
  let rec pit l =
    print p disp l.car;
    match l.cdr with
      Snull -> ()
    | Spair t -> putc p ' '; pit t
    | x -> puts p " . "; print p disp x
  in
  pit l;
  putc p ')';
  up ()

and print p disp x =
  let drop = drop_sublist_flag in
  match x with
      Snull -> drop (); puts p "()"
    | Seof -> drop (); puts p "#<eof>"
    | Strue -> drop (); puts p "#t"
    | Sfalse -> drop (); puts p "#f"
    | Sstring s -> drop (); if disp then puts p s else write_string p s
    | Ssymbol s -> drop (); puts p s
    | Sint i -> drop (); puts p (string_of_int i)
    | Sreal r -> drop (); puts p (string_of_real r)
    | Scomplex z -> drop (); puts p (string_of_complex z)
    | Sbigint b -> drop (); puts p (Big_int.string_of_big_int b)
    | Srational r -> drop (); puts p (Ratio.string_of_ratio r)
    | Schar c -> drop (); if disp then putc p c else write_char p c
    | Spair l -> write_list p disp l
    | Svector v -> drop (); write_vector p disp v
    | Sport _ -> drop (); puts p "#<port>"
    | Sproc _ -> drop (); puts p "#<procedure>"
    | Sprim { prim_fun = _; prim_name = n } -> drop ();
	puts p "#<primitive:"; puts p n; putc p '>'
    | Spromise _ -> drop ();
	puts p "#<promise>"
    | _ -> drop (); puts p "#<unknown>"

let string_of_sval ?(disp=false) v =
  let b =
    Buffer.create 32 in
  print b disp v;
  Buffer.contents b

let sval_of_string s =
  Ocs_read.read_from_string s

exception Bad_sexp of string

let unpair = function
  | Spair v -> v
  | v -> raise (Bad_sexp (string_of_sval v))

let make_string = function
  | Sstring s -> s
  | Ssymbol s -> s
  | v -> raise (Bad_sexp (string_of_sval v))

let make_int = function
  | Sint n -> n
  | Sstring s as v ->
      (try int_of_string s with _ -> raise (Bad_sexp (string_of_sval v)))
  | v -> raise (Bad_sexp (string_of_sval v))

let make_bool = function
  | Strue -> true
  | Sfalse -> false
  | v -> raise (Bad_sexp (string_of_sval v))

let rec iter f = function
  | Spair x -> f x.car; iter f x.cdr
  | Snull -> ()
  | v -> raise (Bad_sexp (string_of_sval v))

let rec map f = function
  | Spair x ->
      let r = f x.car in
      r :: (map f x.cdr)
  | Snull -> []
  | v -> raise (Bad_sexp (string_of_sval v))
    
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
  | Snull -> ()
  | v -> raise (Bad_sexp (string_of_sval v))

let car v = (unpair v).car
let cdr v = (unpair v).cdr

let error v =
  raise (Bad_sexp (string_of_sval ~disp:true v))

let make_list f v =
  let rec scan acc = function
    | Spair v ->
	scan ((f v.car)::acc) v.cdr
    | Snull -> acc
    | x -> error x
  in List.rev (scan [] v)

let read_list =
  map (fun v -> v)

let read_box =
  map (fun v -> v)

let read_record : Ocs_types.sval -> (string * Ocs_types.sval) list =
  map (function
    | Spair v -> 
	(match v.car with 
	  | Ssymbol s -> s
	  | Sstring s -> s
	  | v -> raise (Bad_sexp (string_of_sval v))),
	car v.cdr
    | v       -> raise (Bad_sexp (string_of_sval v)))

let read_tag v =
  let n =
    match car v with 
      | Ssymbol s -> s
      | Sstring s -> s
      | v -> raise (Bad_sexp (string_of_sval (car v)))
  in n, car (cdr v)

let read_tag_list v =
  let n =
    match car v with
      | Ssymbol s -> s
      | Sstring s -> s
      | v -> raise (Bad_sexp (string_of_sval (car v)))
  in n, cdr v

let read_fields: Ocs_types.sval -> (string * Ocs_types.sval) list =
  map (function
    | Spair v -> 
	(match v.car with 
	  | Ssymbol s -> s 
	  | v -> raise (Bad_sexp (string_of_sval v))), v.cdr
    | v       -> raise (Bad_sexp (string_of_sval v)))

let read_section v =
  let t =
    match car v with
      | Ssymbol s -> s
      | v -> raise (Bad_sexp (string_of_sval v)) in
  let n =
    match car (cdr v) with
      | Ssymbol s -> s
      | v -> raise (Bad_sexp (string_of_sval v)) in
  t, n, (read_fields (cdr (cdr v)))

let read_parameter v =
  let (n,v) =
    read_tag_list v in
  n, read_fields v

let read_string = function
  | Sstring s -> s
  | v -> raise (Bad_sexp (string_of_sval v))

let read_symbol = function
  | Ssymbol s -> s
  | v -> raise (Bad_sexp (string_of_sval v))

let read_int = function
  | Sint n -> n
  | v -> raise (Bad_sexp (string_of_sval v))

let read_int64 = function
  | Sint n -> Int64.of_int n
  | Sbigint n -> Big_int.int64_of_big_int n
  | v -> raise (Bad_sexp (string_of_sval v))

let read_float = function
  | Sreal n -> n
  | v -> raise (Bad_sexp (string_of_sval v))

let read_boolean = function
  | Strue -> true
  | Sfalse -> false
  | v -> raise (Bad_sexp (string_of_sval v))

let read_file file =
  Ocs_read.read_from_port (Ocs_port.open_input_port file)

(* Legacy *)

let rec make_scm_list f = function
  | [] -> Snull
  | hd::tl ->
      Spair { car = f hd; cdr = make_scm_list f tl }

let rec make_scm_record r =
  make_scm_list 
    (fun (k,v) -> 
      Spair { car = Ssymbol k; cdr = Spair { car = v ; cdr = Snull } }) r

(* Generation *)
   
let rec gen_list = function
  | [] -> Snull
  | hd::tl ->
      Spair { car = hd; cdr = gen_list tl }

let gen_record l =
  gen_list 
    (List.map 
      (fun (k,v) -> 
	Spair { car = Ssymbol k; cdr = Spair { car = v ; cdr = Snull } }) l)

let gen_tag (hd,fin) =
  Spair 
    {
      car = Ssymbol hd;
      cdr = Spair { car = fin; cdr = Snull } }
  
let gen_tag_list ?(strkey=false) (hd,tl) =
  Spair 
    { 
      car = if strkey then Sstring hd else Ssymbol hd;
      cdr = tl }

let gen_fields ?(strkey=false) l =
  gen_list
    (List.map 
      (fun (k,v) -> 
	Spair { car = if strkey then Sstring k else Ssymbol k; cdr = (gen_list v)}) l)

let gen_section (typ,name,l) =
  Spair {
    car = Ssymbol typ;
    cdr = Spair {
      car = Ssymbol name;
      cdr = (gen_fields l) }}

let gen_parameter ?(strkey=false) ?(arg=false) (n,l) =
  let fields =  gen_fields ~strkey l  in
  gen_tag_list (n, if arg then (gen_list [fields]) else fields)

let gen_int n = Sint n
let gen_int64 n = Sbigint (Big_int.big_int_of_int64 n)
let gen_float n = Sreal n
let gen_boolean n = if n then Strue else Sfalse
let gen_string s = Sstring s
let gen_symbol s = Ssymbol s
let gen_box = gen_list

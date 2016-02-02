open Spectype
open Platform
open Ocs_types
open Printf
open Logger

type os = string
type depend = os * platform list * Spectype.platform_depend list
type depends = depend list

exception Load_error of string

exception No_depends_file of string
exception Bad_depend_record of string
exception Bad_description of string
exception Bad_vstrict of string
 
let parse_vstrict v =
  match Scm.read_list v with
    | op::value::_ ->
	let op =
	  match Scm.read_symbol op with
	    | "=" ->    Pkg_eq
	    | ">" ->    Pkg_gt
	    | "<" ->    Pkg_lt
	    | ">=" ->   Pkg_ge
	    | "<=" ->   Pkg_le
	    | "last" -> Pkg_last 
	    | _      -> raise (Bad_vstrict (Scm.string_of_sval v))
	in
	(op, Scm.read_string value)
    | _ -> raise (Bad_vstrict (Scm.string_of_sval v))

let parse_description v =
  match Scm.read_tag v with
    | "desc", v ->
	Scm.read_string v
    | _ -> raise (Bad_description (Scm.string_of_sval v))

let parse_platform_depend v =
  match Scm.read_list v with
    | pkgname::vstrict::description::_ ->
	Scm.read_string_or_symbol pkgname,
	Some (parse_vstrict vstrict),
	Some (parse_description description)

    | pkgname::description_or_vstrict::_ ->
	(try
	  Scm.read_string_or_symbol pkgname,
	  None,
	  Some (parse_description description_or_vstrict)
	with Bad_description  _ ->
	  Scm.read_string_or_symbol pkgname,
	  Some (parse_vstrict description_or_vstrict),
	  None)

    | pkgname::_ ->
	Scm.read_string_or_symbol pkgname, None, None
    
    | _ ->
	raise (Bad_depend_record (Scm.string_of_sval v))

let parse_depend v =
  match Scm.read_list v with
    | os::platforms::deps ->
	Scm.read_symbol os,
	(List.map platform_of_string 
	  (List.map Scm.read_symbol
	    (Scm.read_list platforms))),
	(List.map parse_platform_depend deps)
    | _ ->
	raise (Bad_depend_record (Scm.string_of_sval v))

let parse file : depends =
  if Sys.file_exists file then
    begin
      let port = 
	Ocs_port.open_input_port file in
      let value =
	Ocs_read.read_from_port port in
      Ocs_port.close port;
      try
	match Scm.read_tag_list value with
	  | "depends", v ->
	      List.map parse_depend (Scm.read_list v)
	  | _ ->
	      raise (No_depends_file file)
      with
	| Bad_depend_record s -> raise (Bad_depend_record (s ^ "(file: " ^ file ^ ")"))
	| Bad_vstrict s       -> raise (Bad_vstrict       (s ^ "(file: " ^ file ^ ")"))
	| Bad_description s   -> raise (Bad_description   (s ^ "(file: " ^ file ^ ")"))
    end
  else []

let write file (depends : depends) =
  let ch = open_out file in
  let out = output_string ch in
  out "(depends\n";
  List.iter 
    (fun (os,platforms,deps) ->
      out (sprintf "  (%s (%s)\n" os (String.concat " " (List.map string_of_platform platforms)));
      List.iter 
	(fun (pkg_name, ov_opt, pkg_desc_opt) ->
	  let pkg_desc =
	    match pkg_desc_opt with
	      | None -> ""
	      | Some desc ->
		  sprintf " (desc \"%s\")" desc
	  in
	  match ov_opt with
	    | Some (op,ver) ->
		out (sprintf "    (\"%s\" (%s \"%s\")%s)\n" pkg_name (string_of_op op) ver pkg_desc)
	    | None ->
		out (sprintf "    (\"%s\"%s)\n" pkg_name pkg_desc))
	deps;
      out "  )\n";
    ) depends;
  out ")\n";
  close_out ch

let print (depends : platform_depend list) =
  print_endline "Use depends:";
  List.iter
    (fun (pkg_name, ov_opt, pkg_desc) ->
      print_endline (sprintf "  - pkg-name(%s), pkg-op(%s), pkg-ver(%s), pkg-desc(%s)" pkg_name
	(match ov_opt with Some ov -> string_of_op (fst ov) | None -> "")
	(match ov_opt with Some ov -> snd ov | None -> "")
	(match pkg_desc with Some s -> s | None -> "")))
    depends

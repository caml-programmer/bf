open Spectype
open Platform
open Ocs_types
open Printf
open Logger

type os = string
type depend = os * Spectype.platform_depend list
type depends = depend list

exception Load_error of string

let parse file : depends  =
  let make_dep v =
    let v2 = Scheme.map (fun v -> v) v in
    try
      let name_v = List.hd v2 in
      let op_ver_v = try Some (List.nth v2 1) with _ -> None in
      let desc_v = try Some (List.nth v2 2) with _ -> None in
      
      let pkg_name = ref None in
      let pkg_op = ref None in
      let pkg_ver = ref None in
      let pkg_desc = ref None in
      
      let add_op op v =
	let ver =
	  Scheme.make_string (Scheme.fst v) in
	pkg_op  := Some op;
	pkg_ver := Some ver;
      in
      
      (match name_v with
	| Ssymbol s -> pkg_name := Some s
	| Sstring s -> pkg_name := Some s
	| _ -> ());
      (match op_ver_v with
	| None -> ()
	| Some op_ver ->
	    Scheme.parse
	      [
		"=",add_op Pkg_eq;
		">",add_op Pkg_gt;
		"<",add_op Pkg_lt;
		">=", add_op Pkg_ge;
		"<=", add_op Pkg_le;
		"last", add_op Pkg_last;
	      ] op_ver);
      (match desc_v with
	| None -> ()
	| Some desc ->
	    Scheme.parse
	      [ "desc", (fun v -> 
		pkg_desc := Some (Scheme.make_string (Scheme.fst v))) ] desc);
      
      (match (!pkg_name : pkg_name option) with
	| Some name ->
	    (match !pkg_op, !pkg_ver with
	      | Some op, Some ver ->
		  (name,(Some (op,ver)),!pkg_desc)
	      | _ ->
		  (name,None,!pkg_desc))
	| None -> raise Not_found)
    with exn ->
      log_message (Printexc.to_string exn);
      log_message "Package value:";
      Scheme.print v;
      log_error "Cannot add package"
  in   
  let add_os v =
    let n = 
      Scheme.make_string (Scheme.fst v) in
    n, (Scheme.map make_dep (Scheme.snd (Scheme.snd v)))
  in
  if Sys.file_exists file then
    begin
      let acc = ref [] in
      Scheme.parse
	["depends",(fun v -> acc := (Scheme.map add_os v))]
	(Ocs_read.read_from_port
	  (Ocs_port.open_input_port file));
      !acc
    end
  else []

let write file (depends : depends) =
  let ch = open_out file in
  let out = output_string ch in
  out "(depends\n";
  List.iter 
    (fun (os,deps) ->
      out (sprintf "  (%s ()\n" os);
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

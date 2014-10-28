open Spectype
open Platform
open Ocs_types
open Printf
open Logger

let load ?snapshot ?(interactive=false) ?(ignore_last=false) file =
  let packdir = Filename.dirname (Filename.dirname (Filename.dirname file)) in
  let acc = ref ([] : depend list) in
  let add_depend v = acc := v::!acc in
  let add_package v =
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
	(match snapshot with
	  | Some (ver',rev') ->
	      (match op with
		| Pkg_last -> 
		    with_platform 
		      (fun _ platform ->
			pkg_ver := Some (sprintf "%s-%s.%s" ver' rev' (string_of_platform platform)))
		| _ ->
		    (match !pkg_name with
		      | Some pkg ->			  
			  if Strings.have_prefix (Params.get_param "pkg-prefix") pkg then
			    pkg_ver := Some ver'
			  else 
			    pkg_ver := Some ver
		      | None ->
			  pkg_ver := Some ver'))
	  | None ->
	      (match op with
		| Pkg_last ->
		    if ignore_last then
		      pkg_ver := Some ver
		    else
		      with_platform 
			(fun os platform ->
			  (try
			    ignore
			      (System.with_dir packdir
				(fun () ->
				  let branch =
				    Specdir.branch (Filename.dirname file) in
				  let specdir =
				    sprintf "%s/%s"
				(match !pkg_name with
				  | Some s -> s
				  | None -> log_error (sprintf "some package name - not found in %s" file))
				      branch
				  in
				  pkg_ver := Some (sprintf "%s-%d.%s" ver
				    (snd (Package.release ~version:ver specdir))
				    (string_of_platform platform))))			
			  with exn ->
			    log_message (sprintf "Warning: %s -> try using local pkg archive for search last pkg revision" (Package.string_of_pkgexn exn));
			    pkg_ver := Some (sprintf "%s-%d.%s" ver
			      (Pkgsearch.revision ~interactive (match !pkg_name with Some s -> s | None -> raise Not_found) ver)
			      (string_of_platform platform))))
		| _ ->
		    pkg_ver := Some ver))
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
		  add_depend (name,(Some (op,ver)),!pkg_desc)
	      | _ ->
		  add_depend (name,None,!pkg_desc))
	| None -> raise Not_found)
    with exn ->
      log_message (Printexc.to_string exn);
      log_message "Package value:";
      Scheme.print v;
      log_error "Cannot add package"
  in   
  let make_platforms v =
    try
      Scheme.map
	(fun x ->
	  platform_of_string
	  (Scheme.make_string x)) v
    with Not_found ->
      log_message "Platforms value:";
      Scheme.print v;
      log_error "Cannot parse platform value";
  in
  let platform_filter v =
    with_platform (fun os platform ->
      let platforms = make_platforms (Scheme.fst v) in
      if platforms = [] || List.mem platform platforms then
	Scheme.iter add_package (Scheme.snd v))
  in
  let add_os =
    Scheme.parse
      (List.filter
	(fun v -> (os_of_string (fst v)) = (os ()))
	[
	  "linux", platform_filter;
	  "sunos", platform_filter;
	])
  in
  if Sys.file_exists file then
    begin
      Scheme.parse
	["depends",(Scheme.iter add_os)]
	(Ocs_read.read_from_port
	  (Ocs_port.open_input_port file));
      !acc
    end
  else []

let parse file =
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

let write file depends =
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

let print depends =
  print_endline "Use depends:";
  List.iter
    (fun (pkg_name, ov_opt, pkg_desc) ->
      print_endline (sprintf "  - pkg-name(%s), pkg-op(%s), pkg-ver(%s), pkg-desc(%s)" pkg_name
	(match ov_opt with Some ov -> string_of_op (fst ov) | None -> "")
	(match ov_opt with Some ov -> snd ov | None -> "")
	(match pkg_desc with Some s -> s | None -> "")))
    depends

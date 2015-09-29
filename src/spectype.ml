open Platform
open Component
open Printf
open List
open String
open Output
open Logger
open Ocs_types
       
(* описание типов *)

type pkg_name = string
type pkg_desc = string
type pkg_ver = string
type pkg_op =
  | Pkg_le
  | Pkg_lt
  | Pkg_eq
  | Pkg_ge
  | Pkg_gt
  | Pkg_last
type platform_depend = pkg_name * (pkg_op * pkg_ver) option * pkg_desc option
type reject = string (* зависимости, которые не надо включать в require (специфика rpmbuild) *)
type provide = string
type spec = {
  pkgname : pkg_name;
  depends : platform_depend list;
  provides : provide list;
  obsoletes : provide list;
  rejects : reject list;
  components : component list;
  pre_install : string option;
  pre_update : string option;
  pre_uninstall : string option;
  post_install : string option;
  params : (string,string) Hashtbl.t;
  hooks : string option;
  }

exception Pack_branch_is_not_found of string
exception Invalid_specdir_format
exception Unsupported_specdir_version of string
					   
(* функции печати типов *)

(* эта функция используется для расставления зависимостей в пакете *)
let string_of_pkg_op = function
  | Pkg_le -> "<="
  | Pkg_lt -> "<"
  | Pkg_eq -> "="
  | Pkg_ge -> ">="
  | Pkg_gt -> ">"
  | Pkg_last -> "="

let string_of_op = function
  | Pkg_last -> "last"
  | v -> string_of_pkg_op v

let string_of_platform_depend (dependency:platform_depend) =
  let (pkg_name, dep, _) = dependency in
  string_of_string_list ~separator:" "
    [
      pkg_name;
      (match dep with
       | Some (op, ver) -> 
	  sprintf " %s %s" (string_of_op op) ver
       | None -> "")
    ]

let op_of_string = function
  | "last" -> Pkg_last
  | "<=" -> Pkg_le
  | "<" -> Pkg_lt
  | "=" -> Pkg_eq
  | ">=" -> Pkg_ge
  | ">" -> Pkg_gt
  | _ as op -> Output.err "Spectype.op_of_string" ("Undefined operator: "^op)
	     
let string_of_spec spec =
  string_of_string_list (
      List.filter
	not_string_empty
	[
	  (sprintf "PKGNAME: %s" spec.pkgname);
	  "DEPENDS:";
	  (prefix_textblock "  " (string_of_string_list
				    (List.map string_of_platform_depend spec.depends)));
	  "PROVIDES:";
	  (prefix_textblock "  " (string_of_string_list spec.provides));
	  "OBSOLETES:";
	  (prefix_textblock "  " (string_of_string_list spec.obsoletes));
	  "REJECTS:";
	  (prefix_textblock "  " (string_of_string_list spec.rejects));
	  "COMPONENTS:";
	  (prefix_textblock "  " (enumerate_string_list
				    (List.map string_of_component spec.components)));
	  (sprintf "PRE_INSTALL: %s" (string_of_string_option spec.pre_install));
	  (sprintf "PRE_UPDATE: %s" (string_of_string_option spec.pre_update));
	  (sprintf "PRE_UNINSTALL: %s" (string_of_string_option spec.pre_uninstall));
	  (sprintf "POST_INSTALL: %s" (string_of_string_option spec.post_install));
	  "PARAMS: TODO";
	  (sprintf "HOOKS: %s" (string_of_string_option spec.hooks));
	])

(* функция загрузки depends *)

exception No_pkg_prefix of string

let depload ?snapshot ?(interactive=false) ?(ignore_last=false) file : platform_depend list =
  let packdir = Filename.dirname (Filename.dirname (Filename.dirname file)) in
  let pkgname = Filename.basename (Filename.dirname (Filename.dirname file)) in
(*
  print_endline ("file: " ^ file);
  print_endline ("packdir: " ^ packdir);
  print_endline ("pkgname: " ^ pkgname);
 *)
  let pkg_prefix =
    try
      let pos = String.index pkgname '-' in
      String.sub pkgname 0 (pred pos)
    with Not_found ->
      raise (No_pkg_prefix pkgname) in

  let acc = ref ([] : platform_depend list) in
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
			  if Strings.have_prefix pkg_prefix pkg then
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
				    (snd (Release.get ~version:ver specdir))
				    (string_of_platform platform))))			
			  with exn ->
			    log_message (sprintf "Warning: %s -> try using local pkg archive for search last pkg revision" (Printexc.to_string exn));
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
	  platform_of_string (Scheme.make_string x)) v
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
      try
	Scheme.parse
	  ["depends",(Scheme.iter add_os)]
	  (Ocs_read.read_from_port
	    (Ocs_port.open_input_port file));
	!acc
      with exn ->
	raise (Composite.Load_error (sprintf "%s: %s\n" file (Printexc.to_string exn)))
    end
  else []

(* функции загрузки spec-а *)

let load_v1 specdir =
  let flist = ["rh.spec";"rh.files";"rh.req"] in
  if System.is_directory specdir &&
    List.for_all
    (fun s ->
      Sys.file_exists (Filename.concat specdir s))
    flist
  then
    List.map (Filename.concat specdir) flist
  else raise Invalid_specdir_format

let load_v2 ?(snapshot=false) ?(short_composite=false) ~version ~revision specdir =
  let f = Filename.concat specdir in
  let pkgname = 
    Filename.basename (Filename.dirname specdir) in
  let pack_branch = Filename.basename specdir in
  let load s =
    if Sys.file_exists s then
      let ch = open_in s in
      let content =
	System.string_of_channel ch in
      close_in ch;
      Some content
    else
      None
  in
  let depends = 
    if snapshot then
      depload ~snapshot:(version,revision) (f "depends")
    else
      depload (f "depends") in
  let rejects =
    let n = f "rejects" in
    if Sys.file_exists n then
      System.list_of_channel (open_in n)
    else []
  in
  let provides =
    let n = f "provides" in
    let p =
      with_platform
	(fun os platform ->
	  sprintf "packbranch-%s = %s-%s-%s.%s" pack_branch pkgname
	  version revision (string_of_platform platform)) in
    if Sys.file_exists n then
      p::(System.list_of_channel (open_in n))
    else [p]
  in
  let obsoletes =
    let n = f "obsoletes" in
    if Sys.file_exists n then
      System.list_of_channel (open_in n)
    else []
  in
  let components =
    Composite.components ~short_composite (f "composite") in
  let pre_install =
    load (f "pre-install") in
  let pre_update =
    load (f "pre-update") in
  let pre_uninstall =
    load (f "pre-uninstall") in
  let post_install =
    load (f "post-install") in
  let params =
    let n = f "params" in
    if Sys.file_exists n then
      Params.read_from_file n
    else Hashtbl.create 0
  in
  let hooks =
    let n = f "hooks.scm" in
    if Sys.file_exists n then
      Some n
    else
      let parent = Filename.dirname specdir in
      let pn = Filename.concat parent "hooks.scm" in
      if Sys.file_exists pn then
	Some pn
      else None
  in
  {
    pkgname = pkgname;
    depends = depends;
    provides = provides;
    obsoletes = obsoletes;
    rejects = rejects;
    components = components;
    pre_install = pre_install;
    pre_update = pre_update;
    pre_uninstall = pre_uninstall;
    post_install = post_install;
    params = params;
    hooks = hooks;
  }

let load_v3 ?(snapshot=false) ~version ~revision specdir =
  load_v2 ~short_composite:true ~snapshot ~version ~revision specdir
	  
(* Функция Spec.load получает на вход директорию specdir и возвращает spec, характеризующий её *)
let load ?(snapshot=false) ~version ~revision specdir = 
  let version_file = Filename.concat specdir "version" in
  match Specdir.get_version version_file with
  | "2.0" -> load_v2 ~snapshot ~version ~revision specdir
  | "3.0" -> load_v3 ~snapshot ~version ~revision specdir
  | _ -> failwith "Unknown version of specdir"


(* release  version *)

let depload_v2_new ?(os=Platform.os ()) ?(platform=Platform.current ()) depfile : platform_depend list =
  let err msg = Output.err "Spectype.depload_v2_new" msg in
  let remove_fst_level alist = List.map (function
					  | Spair x -> x.cdr
					  | _ -> err "Not an alist")
					alist in
  let make_dep dep_scm =
    let dep = Scheme.read_list dep_scm in
    let pkgname = Scheme.make_string (List.nth dep 0) in
    let op_ver_scm = List.nth dep 1 in
    let pkgop = op_of_string (Scheme.make_string (Scheme.first op_ver_scm)) in
    let pkgver = Scheme.make_string (Scheme.second op_ver_scm) in
    let op_ver_opt = Some (pkgop, pkgver) in
    let desc_scm_opt = try Some (List.nth dep 2) with _ -> None in
    let desc_opt = match desc_scm_opt with
      | Some desc_scm -> Some (Scheme.make_string (Scheme.second desc_scm))
      | None -> None in
    (pkgname, op_ver_opt, desc_opt) in

  if Sys.file_exists depfile
  then let deplist = Ocs_read.read_from_port (Ocs_port.open_input_port depfile) in
       let dep_oses = Scheme.assoc "depends" deplist in
       let dep_os = remove_fst_level (Scheme.filter_key (Platform.string_of_os os) dep_oses) in
       let deps_platforms =
	 List.filter (function
		       | Spair {car=platforms_scm; cdr=_} ->
			  (match platforms_scm with
			   | Snull -> true
			   | _ -> 
			      List.mem platform
				       (Scheme.map (fun x -> platform_of_string
							       (Scheme.make_string x))
						   platforms_scm))
		       | sval -> err ("Invalid dependency sval: "^(Scm.string_of_sval sval)))
		     dep_os in
       let deps_list = remove_fst_level deps_platforms in
       let deps = List.flatten (List.map Scheme.read_list deps_list) in
       List.map make_dep deps
  else []

let load_v2_new ?(os=Platform.os ()) ?(platform=Platform.current ()) pkgname version =
  let load_file file =
    try Some (System.string_of_file file)
    with System.File_not_exist _ -> None in
  let components = Composite.components "composite" in
  let depends = depload_v2_new ~platform ~os "depends" in
  let provides = System.list_of_file "provides" in
  let rejects = System.list_of_file "rejects" in
  let obsoletes = System.list_of_file "obsoletes" in
  let pre_install = load_file "pre-install" in
  let pre_update = load_file "pre-update" in
  let pre_uninstall = load_file "pre-uninstall" in
  let post_install = load_file "post-install" in
  let params = Params.read_from_file "params" in
  {
    pkgname = pkgname;
    depends = depends;
    provides = provides;
    obsoletes = obsoletes;
    rejects = rejects;
    components = components;
    pre_install = pre_install;
    pre_update = pre_update;
    pre_uninstall = pre_uninstall;
    post_install = post_install;
    params = params;
    hooks = None; (* атавизм со времён load_v1 *)
  }

let newload ?(os=Platform.os ()) ?(platform=Platform.current ()) pkgname version =
  System.with_dir (Specdir.specdir_by_version pkgname version)
    (fun () ->
     match Specdir.get_version "version" with
     | "2.0" -> load_v2_new pkgname version ~platform ~os
     | _ -> failwith "Not now"
    )


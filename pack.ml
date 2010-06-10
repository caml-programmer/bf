(* pack.ml *)

open Types
open Rules
open Commands
open Logger
open Ocs_env
open Ocs_types
open Printf

type os =
  | Linux
  | SunOS

type pkg_engine =
  | Rpm_build
  | Pkg_trans
  | Deb_pkg

type platform =
  | Rhel3
  | Rhel4
  | Rhel5
  | Cent4
  | Cent5
  | Fedora10
  | Alt
  | Arch
  | Solaris8
  | Solaris9
  | Solaris10
  | Debian

type platform_mapping =
    (string * ((string * platform) list)) list

exception Permanent_error of string

let engine_of_platform = function
  | Rhel3     -> Rpm_build
  | Rhel4     -> Rpm_build
  | Rhel5     -> Rpm_build
  | Cent4     -> Rpm_build
  | Cent5     -> Rpm_build
  | Fedora10  -> Rpm_build
  | Alt       -> Rpm_build
  | Arch      -> Rpm_build
  | Solaris8  -> Pkg_trans
  | Solaris9  -> Pkg_trans
  | Solaris10 -> Pkg_trans
  | Debian    -> Deb_pkg

let string_of_platform = function
  | Rhel3     -> "rhel3"
  | Rhel4     -> "rhel4"
  | Rhel5     -> "rhel5"
  | Cent4     -> "cent4"
  | Cent5     -> "cent5"
  | Fedora10  -> "f10"
  | Alt       -> "alt"
  | Arch      -> "arch"
  | Solaris8  -> "sol8"
  | Solaris9  -> "sol9"
  | Solaris10 -> "sol10"
  | Debian    -> "deb"
     
let platform_of_string = function
  | "rhel3" -> Rhel3
  | "rhel4" -> Rhel4
  | "rhel5" -> Rhel5
  | "cent4" -> Cent4
  | "cent5" -> Cent5
  | "f10"   -> Fedora10
  | "alt"   -> Alt
  | "arch"  -> Arch
  | "sol8"  -> Solaris8
  | "sol9"  -> Solaris9
  | "sol10" -> Solaris10
  | "deb"   -> Debian
  |  s -> log_error (sprintf "Unsupported platform (%s)" s)

let os_of_string = function
  | "linux" -> Linux
  | "sunos" -> SunOS
  | s -> log_error (sprintf "Unsupported OS (%s)" s)
      
let os_as_string = System.uname

let os () =
  os_of_string (os_as_string ())

let linux_platform_mapping =
  [
    "/etc/redhat-release",
    [
      "^Red Hat Enterprise.*?release 3",Rhel3;
      "^Red Hat Enterprise.*?release 4",Rhel4;
      "^Red Hat Enterprise.*?release 5",Rhel5;
      "^CentOS.*?release 4",Cent4;
      "^CentOS.*?release 5",Cent5;
      "^Fedora.*?release 10",Fedora10;
      "^ALT Linux",Alt
    ];
    "/etc/arch-release", ["^.*",Arch];
    "/etc/debian_version",["^.*",Debian];
  ]
    
let rec select_platforms acc = function
  | [] -> acc
  | (file,mapping)::tl ->
      if Sys.file_exists file then
	begin
	  let s = System.read_file ~file in
	  let l = 
	    List.filter
	      (fun (pat,_) -> Pcre.pmatch ~pat s) mapping in
	  (match l with
	    | (_,platform)::_ -> 
		select_platforms (acc @ [platform]) tl
	    | _ -> 
		select_platforms acc tl)
	end
      else select_platforms acc tl

let sunos_platfrom () =
  match System.uname ~flag:'r' () with
    | "5.8"  -> Solaris8
    | "5.9"  -> Solaris9
    | "5.10" -> Solaris10
    |  s     ->	log_error (sprintf "Unsupported SunOS (%s)" s)

let with_platform (f : os -> platform -> 'a) =
  let os = os () in
  match os with
    | Linux ->
	let platform =
	  (match select_platforms [] linux_platform_mapping with
	    | [] -> log_error "unknown or unsupported platform"
	    | p::_ -> p)
	in f os platform
    | SunOS ->
	f os (sunos_platfrom ())

let check_rh_build_env () =
  System.check_commands ["rpmbuild"]

let rpmbuild
  ?(top_label="topdir") ?(top_dir="/tmp/rpmbuild")
  ?(nocopy="/") ?(buildroot=(Filename.concat (Sys.getcwd ()) "buildroot"))
  ?(format="%%{NAME}-%%{VERSION}-%%{RELEASE}.%%{ARCH}.rpm")
  ~pkgname ~platform ~version ~release ~spec ~files ~findreq () =
  let args = ref [] in
  let add s = args := !args @ [s] in
  let define n v =
    add "--define"; add (sprintf "%s %s" n v)
  in
  let arch = System.arch () in
  let location = Sys.getcwd () in
  let rhsys = string_of_platform platform in
  add "-bb";
  add ("--target=" ^ arch);
  add spec;
  define "_rpmdir" location;
  define "fileslist" files; (* must be absolute *)
  define top_label top_dir;
  define "nocopy" nocopy;
  define "buildroot" buildroot;
  define "_build_name_fmt" format;
  define "pkgname" pkgname;
  define "pkgvers" version;
  define "pkgrel" release;
  define "rhsys" rhsys;
  define "findreq" findreq;
  define "_unpackaged_files_terminate_build" "0";

  let cmd = 
    sprintf "rpmbuild %s" 
      (String.concat " " (List.map (fun x -> "'" ^ x ^ "'") !args)) in

  log_message (sprintf "run: %s" cmd);
  
  let fullname =
    sprintf "%s-%s-%s.%s.%s.rpm"
      pkgname version release rhsys arch in
  if Sys.file_exists (Filename.concat location fullname) then
    location,fullname
  else
    begin
      let pid = Unix.fork () in
      if pid > 0 then
	begin
	  log_message (sprintf "waiting for build package as pid %d" pid);
	  try
	    while true do
	      match Unix.waitpid [Unix.WNOHANG] pid with
		| 0,_ -> 
		    Unix.sleep 1
		| n,Unix.WEXITED 0 when n = pid ->
		    raise Not_found
		| _,_ ->
		    log_error
		      (sprintf "Cannot build package: %s/%s" location fullname)
	    done; location,fullname
	  with 
	    | Not_found ->
		location,fullname
	    | exn ->
		log_error
		  (sprintf "Cannot build package[%s]: %s/%s" (Printexc.to_string exn) location fullname)
	end
      else
	begin
	  let _ =
	    Unix.execvp "rpmbuild" (Array.of_list ("rpmbuild"::!args)) in
	  exit 0
	end
    end

type content =
    [
    | `File of string
    | `Dir of string
    | `Empty_dir of string
    | `None
    ]

let dest_dir () =
  let dest_dir = Params.get_param "dest-dir" in
  if dest_dir <> "" then
    Some dest_dir
  else None  

let copy_to_buildroot ?(buildroot=(Filename.concat (Sys.getcwd ()) "buildroot")) ~top_dir files =
  remove_directory buildroot;
  make_directory [buildroot];
  
  let match_prefix p v =
    let pl = String.length p in
    let vl = String.length v in
    vl >= pl && String.sub v 0 pl = p
  in
  let parse_line s =
    let make_path s =
      let n =
	Pcre.replace ~pat:"%dir " ~templ:""
	  (Pcre.replace ~pat:"%topdir" ~templ:top_dir
	    (Pcre.replace
	      ~pat:"%config\\(noreplace\\) %topdir"
	      ~templ:top_dir s))
      in 
      let m = 
	let l = String.length n in
	if l > 0 && n.[0] = '/' then
	  String.sub n 1 (pred l)
	else n
      in m
    in
    let len = String.length s in
    if match_prefix "%dir " s then
      `Empty_dir (make_path s)
    else if match_prefix "%config(noreplace) %topdir" s then
      `File (make_path s)
    else if match_prefix "%nocopy" s || match_prefix "#" s then
      `None
    else
      if s.[pred len] = '/' then
	`Dir (make_path s)
      else
	`File (make_path s)
  in
  let ch = open_in files in
  try
    while true do
      let raw = input_line ch in
      match parse_line raw with
	| `File s ->
	    (try
	      let ns =
		match dest_dir () with
		  | Some d -> Filename.concat d (System.strip_root s)
		  | None   -> s in
	      let dname =
		Filename.concat buildroot (Filename.dirname s) in
	      let src = "/" ^ ns in
	      let dst = Filename.concat buildroot s in
	      System.create_directory_r dname;
	      System.link_or_copy src dst
	    with exn ->
	      log_message ("f " ^ s);
	      raise exn)
	| `Dir s ->
	    (try
	      let ns =
		match dest_dir () with 
		  | Some d -> Filename.concat d (System.strip_root s)
		  | None   -> s in
	      let dname =
		Filename.concat buildroot (Filename.dirname s) in
	      let src = "/" ^ ns in
	      let dst = Filename.concat buildroot s in
	      System.create_directory_r dname;
	      remove_directory (Filename.concat buildroot s);
	      System.copy_dir src dst;
	    with exn ->
	      log_message ("d " ^ s);
	      raise exn)
	| `Empty_dir s ->
	    (try
	      let dst = Filename.concat buildroot s in
	      System.create_directory_r dst
	    with exn ->
	      log_message ("e " ^ s);
	      raise exn)
	| `None ->
	    log_message 
	      (sprintf "copy_to_buildroot: skipped %s" raw)
    done
  with End_of_file -> close_in ch

exception Invalid_specdir_format
exception Unsupported_specdir_version of string

let get_version file =
  let s = System.read_file file in
  try
    String.sub s 0 (String.index s '\n')
  with Not_found -> s

let spec_from_v1 specdir =
  let flist = ["rh.spec";"rh.files";"rh.req"] in
  if System.is_directory specdir &&
    List.for_all
    (fun s ->
      Sys.file_exists (Filename.concat specdir s))
    flist
  then
    List.map (Filename.concat specdir) flist
  else raise Invalid_specdir_format

(* Pkg search *)
   
exception Broken_pkg_iteration of string
exception Cannot_find_pkgver of string
exception Cannot_find_pkgrev of string
exception Revision_must_be_digital of string

let pkgtrans_name_format s =
  try
    let pos = String.index s '-' in
    let r = String.sub s 0 (String.length s) in
    r.[pos] <- 'D';
    for i=0 to pos do
      r.[i] <- Char.uppercase r.[i]
    done; r
  with Not_found -> s

let fix_debian_arch = function
  | "i686" -> "i386"
  | "i586" -> "i386"
  | s -> s

let tag_extraction_rex pkgname = function
  | Debian ->
      Pcre.regexp
	(pkgname ^ "-([^-]+)-(\\d+)\\." ^ (fix_debian_arch (System.arch ())) ^ "\\.deb")
  | platform ->
      Pcre.regexp
	(pkgname ^ "-([^-]+)-(\\d+)\\." ^ (string_of_platform platform) ^ "\\." ^ (System.arch ()) ^ "\\.")

let map_pkg f pkgname =
  try
    with_platform
      (fun os platform ->
	let pkgname =
	  match engine_of_platform platform with
	    | Rpm_build 
	    | Deb_pkg   -> pkgname
	    | Pkg_trans -> pkgtrans_name_format pkgname
	in
	let rex = tag_extraction_rex pkgname platform in
	let ff acc s =
	  if Pcre.pmatch ~rex s then
	    let a = Pcre.extract ~rex s in
	    if Array.length a > 2 then
	      (a.(1),int_of_string a.(2))::acc
	    else acc
	  else acc
	in
	List.map f
	  (List.fold_left ff []
	    (System.list_of_directory (Sys.getcwd ()))))
  with exn ->
    raise (Broken_pkg_iteration (Printexc.to_string exn))

let filter_pkg f pkgname =
  List.filter f
    (map_pkg (fun x -> x) pkgname)

let rec read_number max =
  print_string "> "; flush stdout;
  try
    let s = input_line stdin in
    let n = int_of_string s in
    if max <> 0 && n > max then
      raise Not_found
    else n
  with _ ->
    read_number max

let read_string () =
  print_string "> "; flush stdout;
  input_line stdin

let find_pkg_version ?(interactive=false) pkgname =
  try
    (match
      List.sort 
	(fun a b -> compare b a)
	(map_pkg fst pkgname)
    with [] -> raise Not_found
      | hd::tl -> hd)
  with exn ->
    if interactive then
      begin
	log_message (sprintf "Enter package version for %s" pkgname);
	read_string ()
      end
    else
      raise (Cannot_find_pkgver (Printexc.to_string exn))

let find_pkg_revision ?(interactive=false) pkgname version =
  try
    (match
      List.sort
	(fun a b -> compare b a)
	(List.map snd
	  (filter_pkg
	    (fun (ver,_) -> ver = version) pkgname))
    with [] -> raise Not_found
      | hd::tl -> hd)
  with exn ->
    if interactive then
      begin
	log_message (sprintf "Enter package revision for %s-%s" pkgname version);
	read_number 0	
      end
    else
      raise (Cannot_find_pkgrev (Printexc.to_string exn))

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
    
type depend = 
    pkg_name * (pkg_op * pkg_ver) option * pkg_desc option

type reject =
    string (* pcre pattern *)

type provide =
    string (* string symbol *)

type spec = {
  pkgname : pkg_name;
  depends : depend list;
  provides : provide list;
  rejects : reject list;
  components : component list;
  pre_install : string option;
  pre_update : string option;
  pre_uninstall : string option;
  post_install : string option;
  params : (string,string) Hashtbl.t;
  hooks : string option;
}

let string_of_pkg_op = function
  | Pkg_le -> "<="
  | Pkg_lt -> "<"
  | Pkg_eq -> "="
  | Pkg_ge -> ">="
  | Pkg_gt -> ">"
  | Pkg_last -> "="

let make_depends ?(interactive=false) ?(ignore_last=false) file =
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
	(match op with
	  | Pkg_last ->
	      if ignore_last then
		pkg_ver := Some ver
	      else
		with_platform 
		  (fun os platform ->
		    (try
		      ignore(with_component_dir
			~strict:false (make_component ~label:(Branch "master") "pack")
			(fun () ->
			  let branch = 
			    branch_of_specdir (Filename.dirname file) in
			  let specdir =
			    sprintf "%s/%s"
			      (match !pkg_name with
				| Some s -> s
				| None -> log_error (sprintf "some package name - not found in %s" file))
			      branch
			  in
			  pkg_ver := Some (sprintf "%s-%d.%s" ver
			    (snd (read_pkg_release ~version:ver specdir))
			    (string_of_platform platform))))
		    with exn ->
		      log_message (sprintf "Warning: %s -> try using local pkg archive for search last pkg revision" (Printexc.to_string exn));
		      pkg_ver := Some (sprintf "%s-%d.%s" ver
			(find_pkg_revision ~interactive (match !pkg_name with Some s -> s | None -> raise Not_found) ver)
			(string_of_platform platform))))
	  | _ ->
	      pkg_ver := Some ver)
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

let spec_from_v2 ~version ~revision specdir =
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
  let depends = make_depends (f "depends") in
  let rejects =
    let n = f "rejects" in
    if Sys.file_exists n then
      begin
	let ch = open_in n in
	let patterns = 
	  System.list_of_channel ch in
	close_in ch; patterns
      end
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
      begin
	let ch = open_in n in
	let symbols =
	  System.list_of_channel ch in
	close_in ch; (p::symbols)
      end
    else [p]
  in
  let components =
    components_of_composite (f "composite") in
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
    rejects = rejects;
    components = components;
    pre_install = pre_install;
    pre_update = pre_update;
    pre_uninstall = pre_uninstall;
    post_install = post_install;
    params = params;
    hooks = hooks;
  }

let print_depends depends =
  print_endline "Use depends:";
  List.iter
    (fun (pkg_name, ov_opt, pkg_desc) ->
      print_endline (sprintf "  - pkg-name(%s), pkg-op(%s), pkg-ver(%s), pkg-desc(%s)" pkg_name
	(match ov_opt with Some ov -> string_of_pkg_op (fst ov) | None -> "")
	(match ov_opt with Some ov -> snd ov | None -> "")
	(match pkg_desc with Some s -> s | None -> "")))
    depends

let call_after_build ~location ~fullname hooks =
  Rules.load_plugins ();
  (match hooks with
    | Some file -> Scheme.eval_file file
    | None -> ());
  if Scheme.defined "after-build" then
    Scheme.eval_code (fun _ -> ())
      (sprintf "(after-build \"%s\" \"%s\" \"%s\")"
	(System.hostname ()) location fullname)

let call_before_build ~pkgname ~version ~revision ~platform hooks =
  Rules.load_plugins ();
  (match hooks with
    | Some file -> Scheme.eval_file file
    | None -> ());
  if Scheme.defined "before-build" then
    let result = ref [] in
    Scheme.eval_code (fun v ->
      result := Scheme.map Scheme.make_string v)
      (sprintf "(before-build \"%s\" \"%s\" \"%s\" \"%s\")"
	pkgname version revision (string_of_platform platform));
    !result
  else []
      
let build_over_rpmbuild params =
  let (pkgname,platform,version,release,spec,files,findreq,hooks) = params in
  let top_dir = Params.get_param "top-dir" in
  check_rh_build_env ();
  log_command "chmod" ["+x";findreq];
  copy_to_buildroot ~top_dir files;
  let (location,fullname) =
    rpmbuild
      ~top_dir
      ~pkgname ~platform ~version ~release
      ~spec ~files ~findreq ()
  in call_after_build ~location ~fullname hooks

let check_composite_depends spec =
  let composite_depends =
    List.map
      (fun c ->
	match c.pkg with
	  | Some pkg -> pkg
	  | None -> assert false)
      (List.filter
	(fun c -> c.pkg <> None)
	spec.components)
  in
  let spec_depends =
    List.map 
      (fun (pkg_name,_,_) -> pkg_name) spec.depends
  in
  let rec make acc = function
    | [] -> acc
    | hd::tl ->
	if not (List.mem hd spec_depends) then
	  make (hd::acc) tl
	else
	  make acc tl
  in
  let missings = make [] composite_depends in
  if missings <> [] then
    begin
      List.iter 
	(fun pkg ->
	  log_message (sprintf "package (%s) is missing in depends file" pkg))
	missings;
      raise (Permanent_error "you must correct depends or composite files")
    end

let rpm_key_format s =
  let l = String.length s in
  if l > 0 then
    let r = String.sub s 0 l in
    r.[0] <- Char.uppercase r.[0]; r
  else s
    
let resolve_params find s =
  Pcre.substitute
    ~pat:"%\\(.*?\\)"
    ~subst:(fun s ->
      let l = String.length s in
      let k = String.sub s 2 (l - 3) in
      (try find k with Not_found -> s))
    s

let build_package_impl os platform args =
  match args with
    | [specdir;version;release] ->
	let abs_specdir =
	  if String.length specdir > 0 && specdir.[0] = '/' then
	    specdir
	  else
	    Filename.concat (Sys.getcwd()) specdir
	in
	let with_specdir = Filename.concat abs_specdir in
	let with_out s f =
	  let n = with_specdir s in
	  let ch = open_out n in
	  f (output_string ch);
	  close_out ch; n
	in
	(match get_version (with_specdir "version") with
	  | "1.0" ->
	      (match spec_from_v1 abs_specdir with
		  [spec;files;findreq] ->
		    let pkgname = 
		      Filename.basename specdir in
		    let hookfile =
		      Filename.concat abs_specdir "hooks.scm" in
		    let hooks =
		      if Sys.file_exists hookfile then
			Some hookfile
		      else None
		    in build_over_rpmbuild
			 (pkgname,platform,version,release,spec,files,findreq,hooks)
		| _-> assert false)
	  | "2.0" ->
	      let spec = spec_from_v2 ~version ~revision:release abs_specdir in
	      let bf_table = Hashtbl.create 32 in
	      let reg k =
		if Hashtbl.mem bf_table k then "" 
		else 
		  begin
		    Hashtbl.add bf_table k false;
		    k
		  end
	      in
	      let accumulate_lists add out =
		List.iter
		  (fun c ->
		    let name = c.name in
		    let bf_list =
		      Filename.concat name ".bf-list" in
		    let rec add_with_check () =
		      if Sys.file_exists bf_list then
			add out bf_list
		      else
			begin
			  log_message
			    (sprintf "bf list for (%s) is not found -> need installing" name);
			  let tag =
			    let k =
			      mk_tag (pkgname_of_specdir abs_specdir) version (int_of_string release) in
			    let tag_exists = ref false in
			    with_component_dir ~strict:false c
			      (fun () -> 
				tag_exists := List.mem k (Git.git_tag_list ()));
			    if !tag_exists then
			      Some k
			    else None
			  in
			  reinstall (with_tag tag [c]);
			  add_with_check ()
			end
		    in add_with_check ())
		  (List.filter 
		    (fun c -> c.pkg = None)
		    spec.components)
	      in

	      print_depends spec.depends;
	      check_composite_depends spec;
	      
	      let add_bf_list custom out file =
		let ch = open_in file in
		let rec read () =
		  try
		    out (custom (input_line ch));
		    read ()
		  with End_of_file -> close_in ch
		in read ()
	      in
	      
	      (match engine_of_platform platform with
		| Rpm_build ->
		    let custom_pkg_files =
		      call_before_build
			~pkgname:spec.pkgname ~version ~revision:release ~platform spec.hooks in
		    
		    let files =
		      with_out "rpmbuild.files"
			(fun out ->
			  let make_rpm_line s =
			    let l = String.length s in
			    if l > 2 then			      
			      (match s.[0] with
				| 'd' -> reg (sprintf "%%dir %s\n" (String.sub s 2 (l - 2)))
				| 'f' -> reg (sprintf "%s\n" (String.sub s 2 (l - 2)))
				| _ -> "")
			    else ""
			  in
			  accumulate_lists (add_bf_list make_rpm_line) out;
			  List.iter (fun s -> out (make_rpm_line s))
			    custom_pkg_files)
		    in
		    let findreq =
		      with_out "rpmbuild.findreq"
			(fun out -> 
			  out "#!/bin/sh\n";
			  List.iter 
			    (fun (pkg_name,ov_opt,_) ->
			      (match ov_opt with
				| Some (op,ver) ->
				    out (sprintf "echo \"%s %s %s\"\n"
				      pkg_name (string_of_pkg_op op) ver)
				| None ->
				    out (sprintf "echo %s\n" pkg_name)))
			    spec.depends;
			  let freq =
			    "/usr/lib/rpm/find-requires" in
			  if Sys.file_exists freq then
			    begin
			      out "/usr/lib/rpm/find-requires";
			      List.iter
				(fun reject ->
				  out (sprintf "\\\n| sed -e 's#%s##'" reject))
				spec.rejects
			    end
			  else
			    log_message (sprintf "warning: %s is not found" freq))
		    in
		    let specify_provides l =
		      if System.arch () = "x86_64" then
			List.map
			  (fun p ->
			    if Pcre.pmatch ~pat:"^lib" p then
			      p ^ "()(64bit)"
			    else p) l
		      else l
		    in		    
		    let specfile =
		      with_out "rpmbuild.spec"
			(fun out ->
			  let find_value = function
			    | "topdir" -> Params.get_param "top-dir"
			    | "name" -> spec.pkgname
			    | "version" -> version
			    | "release" -> release ^ "." ^ (string_of_platform platform)
			    | "buildroot" -> "buildroot"
			    | "provides" ->
				String.concat ", " (specify_provides spec.provides)
			    | k -> Hashtbl.find spec.params k
			  in
			  let gen_param k =
			    (try
			      out (sprintf "%s: %s\n" (rpm_key_format k) (find_value k))
			    with Not_found -> ())
			  in
			  gen_param "summary";
			  gen_param "name";
			  gen_param "version";
			  gen_param "release";
			  gen_param "license";
			  gen_param "vendor";
			  gen_param "group";
			  gen_param "url";
			  gen_param "buildroot";
			  if spec.provides <> [] then
			    gen_param "provides";
			  
			  out "%define _use_internal_dependency_generator 0\n";
			  out "%define __find_requires %findreq\n";
			  out "%description\n";
			  out "%files\n";
			  out (sprintf "%%include %s\n" files);

			  let oo s = out (resolve_params find_value s); out "\n" in
			  
			  (match spec.pre_install with
			    | None -> ()
			    | Some pre ->
				out "%pre\n";
				out "if [ \"$1\" = \"1\" ] ; then\n";
				out "echo -n\n";
				oo pre;
				out "fi\n";
				(match spec.pre_update with
				  | None -> ()
				  | Some preup ->
				      out "if [ \"$1\" = \"2\" ] ; then\n";
				      out "echo -n\n";
				      oo preup;
				      out "fi\n"));
			  (match spec.post_install with
			    | None -> ()
			    | Some post ->
				out "%post\n";
				oo post);
			  (match spec.pre_uninstall with
			    | None -> ()
			    | Some preun ->
				out "%preun\n";
				out "if [ \"$1\" = \"0\" ] ; then\n";
				out "echo -n\n";
				oo preun;
				out "fi\n"))
		    in
		    
		    build_over_rpmbuild 
		      (spec.pkgname,platform,version,release,specfile,files,findreq,spec.hooks)
		      
		| Pkg_trans ->
		    let custom_pkg_files =
		      call_before_build
			~pkgname:spec.pkgname ~version ~revision:release ~platform spec.hooks in
		    
		    let pkgtrans_key_format = String.uppercase in
		    let find_value = function
		      | "topdir" -> Params.get_param "top-dir"
		      | "pkg" -> pkgtrans_name_format spec.pkgname
		      | "arch" -> System.arch ()
		      | "version" -> sprintf "%s-%s" version release
		      | "category" -> Hashtbl.find spec.params "group"
		      | "name" -> Hashtbl.find spec.params "summary"
		      | k -> Hashtbl.find spec.params k
		    in
		    let pkginfo =
		      with_out "pkginfo"
			(fun out -> 
			  let gen_param k =
			    (try
			      out (sprintf "%s=%s\n" (pkgtrans_key_format k) (find_value k))
			    with Not_found -> ())
			  in
			  gen_param "pkg";
			  gen_param "arch";
			  gen_param "name";
			  gen_param "version";
			  gen_param "vendor";
			  gen_param "category";
			  gen_param "email")
		    in
		    let prototype =
		      with_out "prototype"
			(fun out ->
			  let make_pkgtrans_line s =
			    let l = String.length s in
			    if l > 2 then
			      (match s.[0] with
				| 'd' ->
				    let dir = String.sub s 2 (l - 2) in
				    let mode = sprintf "%o" (Unix.stat dir).Unix.st_perm in
				    reg (sprintf "d none %s %s root root\n" dir mode)
				| 'f' -> 
				    let file = String.sub s 2 (l - 2) in
				    let mode = sprintf "%o" (Unix.stat file).Unix.st_perm in
				    reg (sprintf "f none %s %s root root\n" file mode)
				| _ -> "")
			    else ""
			  in
			  let write_content name content =
			    let file =
			      Filename.concat abs_specdir name in
			    out (sprintf "i %s=%s\n" name file);
			    System.write_string
			      ~file ~string:(resolve_params find_value content)
			  in
			  let make_depends depends =
			    let b = Buffer.create 32 in
			    let out = Buffer.add_string b in
			    List.iter 
			      (fun (pkg_name,_,pkg_desc_opt) ->
				let pkg_desc =
				  match pkg_desc_opt with
				    | None -> pkg_name
				    | Some s -> s
				in out (sprintf "P %s %s\n" (pkgtrans_name_format pkg_name) pkg_desc))
			      depends;
			    Buffer.contents b
			  in
			  
			  out (sprintf "i pkginfo=%s/pkginfo\n" abs_specdir);
			  
			  (match spec.pre_install with
			    | None -> ()
			    | Some content ->
				(match spec.pre_update with
				  | None ->
				      write_content "preinstall" content
				  | Some upd ->
				      write_content "preinstall"
					(content ^ "\n" ^  upd)));
			  (match spec.post_install with
			    | None -> ()
			    | Some content ->
				write_content "postinstall" content);
			  (match spec.pre_uninstall with
			    | None -> ()
			    | Some content ->
				write_content "preremove" content);
			  (match spec.depends with
			    | [] ->  ()
			    | list ->
				write_content "depend" (make_depends list));
			  
			  accumulate_lists (add_bf_list make_pkgtrans_line) out;
			  List.iter (fun s -> out (make_pkgtrans_line s))
			    custom_pkg_files)
		    in
			
		    let pkg_spool = "/var/spool/pkg/" in
		    let pkg_name = find_value "pkg" in
		    let pkg_file = sprintf "%s-%s.%s.%s" 
		      pkg_name (find_value "version")
		      (string_of_platform platform)
		      (find_value "arch")
		    in
		    let pkg_file_abs = Filename.concat pkg_spool pkg_file in
		    let pkg_file_gz = pkg_file ^ ".gz" in
		    
		    Rules.remove_directory (Filename.concat pkg_spool pkg_name);

		    with_dir abs_specdir
		      (fun () ->
			let root =
			  match dest_dir () with
			    | Some d -> d
			    | None -> "/" in
			log_command "pkgmk"
			  ["-o";"-r";root];
			log_command "pkgtrans" 
			  ["-o";"-s";pkg_spool; pkg_file; pkg_name]);
		    
		    log_command "mv" ["-f";pkg_file_abs;"./"];
		    (try Sys.remove pkg_file_gz with _ -> ());
		    log_command "gzip" [pkg_file];
		    call_after_build 
		      ~location:(Sys.getcwd ())
		      ~fullname:pkg_file_gz spec.hooks
		| Deb_pkg ->
		    let custom_pkg_files =
		      call_before_build
			~pkgname:spec.pkgname ~version ~revision:release ~platform spec.hooks in
		    
		    let make_debian_depends deps =
		      let b = Buffer.create 32 in
		      let add s =
			if Buffer.length b = 0 then
			  Buffer.add_string b s
			else
			  begin
			    Buffer.add_string b ", ";
			    Buffer.add_string b s
			  end
		      in
		      List.iter
			(fun (pkgname, ov_opt, _) ->
			  match ov_opt with
			    | Some (op,ver) ->
				add (sprintf "%s (%s %s)" pkgname (string_of_pkg_op op) ver)
			    | None ->
				add pkgname)
			deps;
		      Buffer.contents b
		    in
		    let find_value = function
		      | "topdir" -> Params.get_param "top-dir"
		      | "source" -> spec.pkgname
		      | "package" -> spec.pkgname
		      | "priority" -> "optional"
		      | "maintainer" -> Hashtbl.find spec.params "email"
		      | "architecture" -> fix_debian_arch (System.arch ())
		      | "version" -> sprintf "%s-%s" version release
		      | "section" -> Hashtbl.find spec.params "group"
		      | "description" -> Hashtbl.find spec.params "summary"
		      | "depends" -> make_debian_depends spec.depends
		      | k -> Hashtbl.find spec.params k
		    in
		    			
		    let make_date () =
		      let tm = Unix.localtime (Unix.time ()) in
		      sprintf "%04d-%02d-%02d" 
			(tm.Unix.tm_year + 1900)
			(tm.Unix.tm_mon + 1)
			(tm.Unix.tm_mday)
		    in
		    
		    let make_deb_line s =
		      let l = String.length s in
		      if l > 2 then
			(match s.[0] with
			  | 'd' ->
			      let dir =
				Filename.concat 
				  (Filename.concat abs_specdir "debian")
				  (System.strip_root (String.sub s 2 (l - 2))) in
			      make_directory [dir];
			  | 'f' ->
			      let src = String.sub s 2 (l - 2) in
			      let dst = Filename.concat 
				(Filename.concat abs_specdir "debian") 
				(System.strip_root src) in
			      System.link_or_copy
				(match dest_dir () with
				  | Some d -> Filename.concat d src
				  | None -> src) dst
			  | _ -> ())
		    in
		    let debian_home =
		      Filename.concat abs_specdir "debian" in
		    remove_directory debian_home;
		    make_directory [debian_home];
		    accumulate_lists (add_bf_list make_deb_line) (fun _ -> ());
		    List.iter make_deb_line custom_pkg_files;
		    		    
		    let write_script name content =
		      let file =
			Filename.concat 
			  (Filename.concat abs_specdir "debian/DEBIAN") name in
		      System.write_string
			~file ~string:(resolve_params find_value content);
		      Unix.chmod file 0o755
		    in

		    with_dir abs_specdir
		      (fun () ->
			let doc_location =
			  sprintf "debian/usr/share/doc/%s" spec.pkgname in
			let man_location =
			  sprintf "debian/usr/share/man/man1" in
			
			make_directory [
			  "debian/DEBIAN";
			  man_location;
			  doc_location;
			];			
			
			(match spec.pre_install with
			  | None -> ()
			  | Some content ->
			      (match spec.pre_update with
				| None ->
				    write_script "preinst" content
				| Some upd ->
				    write_script "preinst"
				      (content ^ "\n" ^  upd)));
			(match spec.post_install with
			  | None -> ()
			  | Some content ->
			      write_script "postinst" content);
			(match spec.pre_uninstall with
			  | None -> ()
			  | Some content ->
			      write_script "prerm" content);

			let copyright =
			  with_out (Filename.concat doc_location "copyright")
			    (fun out ->
			      out (find_value "email"))
			in			
			let manpage =
			  with_out
			    (Filename.concat man_location (spec.pkgname ^ ".1"))
			    (fun out ->
			      out (sprintf ".TH %s 1 \"%s\"\n" spec.pkgname (make_date ()));
			      out ".SH NAME\n";
			      out (sprintf "%s - debian package\n" spec.pkgname);
			      out ".SH DESCRIPTION\n";
			      out (find_value "description");
			      out "\n";
			      out ".SH AUTHOR\n";
			      out (find_value "email");
			      out "\n")
			in
			let control =
			  with_out "debian/DEBIAN/control"
			    (fun out ->
			      let gen_param k =
				(try
				  out (sprintf "%s: %s\n" (rpm_key_format k) (find_value k))
				with Not_found -> ())
			      in
			      gen_param "source";
			      gen_param "section";
			      gen_param "priority";
			      gen_param "maintainer";
			      gen_param "version";
			      gen_param "package";
			      gen_param "architecture";
			      gen_param "depends";
			      gen_param "description";
			      out
				(" " ^ (find_value "description") ^ "\n"))
			in
			let changelog =
			  with_out (Filename.concat doc_location "changelog")
			    (fun out ->
			      out (sprintf "%s (%s-%s) unstable; urgency=low\n" spec.pkgname version release);
			      out "\n";
			      out " * Current Release.\n";
			      out "\n";
			      out
				(sprintf "-- %s %s\n" 
				  (find_value "email")
				  (List.hd (System.read_lines "date -R"))))
			in
			let changelog_deb =
			  with_out (Filename.concat doc_location "changelog.Debian")
			    (fun out ->
			      out (sprintf "%s (%s-%s) unstable; urgency=low\n" spec.pkgname version release);
			      out "\n";
			      out " * Current Release.\n";
			      out "\n";
			      out
				(sprintf "-- %s %s\n"
				  (find_value "email")
				  (List.hd (System.read_lines "date -R"))))
			in
			
			log_command "gzip" ["--best";manpage];
		    	log_command "gzip" ["--best";changelog];
			log_command "gzip" ["--best";changelog_deb];
			log_command "fakeroot" ["dpkg-deb";"--build";"debian"]);
		    
		    let pkgfile =
		      sprintf "%s-%s-%s.%s.deb" 
			spec.pkgname version release (fix_debian_arch (System.arch ())) in
		    log_command
		      "mv" [(Filename.concat abs_specdir "debian.deb");pkgfile])
	  | version ->
	      raise (Unsupported_specdir_version version))
    | _ -> log_error "build_rh_package: wrong arguments"

let build_package args =
  with_platform
    (fun os platfrom ->
      build_package_impl os platfrom args)

exception Bad_specdir of string

let check_specdir specdir =
  try
    ignore(pkgname_of_specdir specdir);
    ignore(branch_of_specdir specdir);
    let p =
      Filename.basename
	(Filename.dirname 
	  (Filename.dirname specdir)) in
    if p <> "pack" && p <> "pack.git" then
      raise (Bad_specdir specdir)
  with _ ->
    raise (Bad_specdir specdir)

let check_pack () =
  with_component_dir ~strict:false (make_component "pack")
    (fun () ->
      (match Git.git_current_branch () with
	| Some "master" ->
	    Git.git_pull "origin";
	| _ ->
	    Git.git_checkout
	      ~force:true ~key:"master" ();
	    Git.git_pull "origin";
	    log_error "current pack branch is not master, bf fix it, try againg"))
	      
let update ~specdir ?(lazy_mode=false) ?(interactive=false) ?(ver=None) ?(rev=None) () =
  let specdir = System.path_strip_directory specdir in
  
  check_specdir specdir;

  let pkgname = pkgname_of_specdir specdir in
  let branch = branch_of_specdir specdir in

  let have_pack_changes =
    update_pack ~specdir (make_component ~label:(Branch "master") "pack") in

  let conv_revision r =
    try int_of_string r with _ -> raise (Revision_must_be_digital r) in
  let (version,revision) =
    (try
      (match ver with
	| Some v ->
	    v, (match rev with Some r -> conv_revision r | None -> log_error (sprintf "cannot update %s: revision does not set" pkgname))
	| None ->
	    read_pkg_release ~next:true specdir)
    with Pkg_release_not_found _ ->
      log_message (sprintf "Warning: Try using local pkg archive for search next package (%s %s) release" pkgname branch);
      let ver' =
	match ver with
	  | Some v -> v
	  | None -> find_pkg_version ~interactive pkgname
      in
      let rev' =
	match rev with
	  | Some r -> conv_revision r
	  | None -> succ (find_pkg_revision ~interactive pkgname ver')
      in (ver',rev'))
  in
  let tag =
    mk_tag pkgname version revision in
  
  let old_tag =
    if  revision > 0 then
      Some (mk_tag pkgname version (pred revision))
    else None
  in

  let composite =
    Filename.concat specdir "composite" in

  let have_composite_changes =
    update_composite composite in

  let build () =
    if not (tag_ready ~tag composite) then
      begin
	install_composite composite;
	tag_composite composite tag;
      end;
    
    install_composite ~tag composite;
    (try
      build_package
	[specdir;version;string_of_int revision];
      reg_pkg_release specdir version revision
    with
      | Permanent_error s ->
	  reg_pkg_release specdir version revision;
	  log_error s;
      | exn -> log_error (Printexc.to_string exn));
    
    (match old_tag with
      | Some old ->
	  (try
	    changelog_composite composite old tag
	  with exn ->
	    log_message (Printexc.to_string exn))
      | None -> ());
    true
  in

  if lazy_mode && not have_composite_changes && not have_pack_changes then
    (log_message "lazy update: noting to do"; false)
  else
    begin    
      log_message 
	(sprintf "start update: lazy-mode(%b), composite-changes(%b), pack-changes(%b)"
	  lazy_mode have_composite_changes have_pack_changes);
      build ()
    end

(* Clone suport *)

type pack_branch = string

type dep_tree =
  | Dep_val of (string * pack_branch * version * revision * string)
  | Dep_list of dep_tree list

exception Cannot_extract_arch of string
exception Cannot_extract_platform of string
exception Cannot_resolve_dependes of string
exception Cannot_extract_revision of string
exception Cannot_extract_version of string
exception Cannot_extract_extension of string
exception Cannot_extract_pkgname of string
exception Pack_branch_is_not_found of string

let extract_extension pkg_name =
  try
    let pos = String.rindex pkg_name '.' in
    String.sub pkg_name (succ pos) (String.length pkg_name - pos - 1)
  with _ ->
    raise (Cannot_extract_extension pkg_name)

let extract_arch pkg_name =
  try
    let pos1 = String.rindex pkg_name '.' in
    let pos2 = String.rindex_from pkg_name (pred pos1) '.' in
    String.sub pkg_name (succ pos2) (pos1 - pos2 - 1)
  with _ ->
    raise (Cannot_extract_arch pkg_name)

let extract_platform pkg_name =
  try
    let pos0 = String.rindex pkg_name '.' in
    let pos1 = String.rindex_from pkg_name (pred pos0) '.' in
    let pos2 = String.rindex_from pkg_name (pred pos1) '.' in
    platform_of_string 
      (String.sub pkg_name (succ pos2) (pos1 - pos2 - 1))
  with _ ->
    raise (Cannot_extract_platform pkg_name)

let extract_revision rest pkg_name =
  try
    let rest_len = String.length rest in
    let s = 
      String.sub pkg_name 0
	(String.length pkg_name - rest_len) in
    let pos = String.rindex s '-' in
    int_of_string 
      (String.sub s (succ pos)
	(String.length s - pos - 1))
  with _ ->
    raise (Cannot_extract_revision pkg_name)

let extract_version rest pkg_name =
  try
    let rest_len = String.length rest in
    let s = 
      String.sub pkg_name 0
	(String.length pkg_name - rest_len) in
    let pos = String.rindex s '-' in
    String.sub s (succ pos)
      (String.length s - pos - 1)
  with _ ->
    raise (Cannot_extract_version pkg_name)

let extract_name rest pkg =
  try
    String.sub pkg 0 (String.length pkg - String.length rest)
  with _ -> raise (Cannot_extract_pkgname pkg)    

let rec get_depends ?(overwrite=false) table acc userhost pkg_path =
  log_message (sprintf "resolve %s" pkg_path);
  let pkg_dir = Filename.dirname pkg_path in
  let pkg = Filename.basename pkg_path in
  let platform = extract_platform pkg in
  let extension = extract_extension pkg in
  let arch = extract_arch pkg in
  let revision =
    extract_revision (sprintf ".%s.%s.%s" (string_of_platform platform) arch extension) pkg in
  let version = 
    extract_version (sprintf "-%d.%s.%s.%s" revision (string_of_platform platform) arch extension) pkg in
  let pkg_name =
    extract_name (sprintf "-%s-%d.%s.%s.%s" version revision (string_of_platform platform) arch extension) pkg in
  let pack_branch =
    match (System.read_lines
      ~filter:(fun s -> 
	Pcre.pmatch ~pat:"packbranch-" s)
      (sprintf "ssh %s rpm -qp --provides %s" userhost pkg_path))
    with [] -> raise (Pack_branch_is_not_found pkg_path)
      | hd::_ ->
	  let pos = String.index hd '-' in
	  let len = 
	    try
	      String.index hd ' '	      
	    with Not_found -> String.length hd
	  in
	  String.sub hd (succ pos) (len - pos - 1)
  in
  let current =
    if overwrite then
      [Dep_val (pkg_name,pack_branch,version,revision,pkg_path)]
    else
      with_platform 
	(fun os platform ->
	  let cur_pkg_name = 
	    sprintf "%s-%s-%d.%s.%s.%s" 
	      pkg_name version revision (string_of_platform platform) (System.arch ()) extension in
	  if Sys.file_exists cur_pkg_name then
	    begin
	      log_message (sprintf "\tpackage %s/%s already exists" (Sys.getcwd ()) cur_pkg_name);
	      []
	    end
	  else
	    [Dep_val (pkg_name,pack_branch,version,revision,pkg_path)])
  in
  let rex = 
    Pcre.regexp "([^\\ ]+)\\s+=\\s+([^-]+)-(\\d+)\\." in
  Dep_list
    (current @ (List.map
      (fun s ->
	let a = Pcre.extract ~rex s in
	let pkg_name = a.(1) in
	let ver = a.(2) in
	let rev = try int_of_string a.(3) with _ -> raise (Cannot_extract_revision pkg_path) in
	if Hashtbl.mem table pkg_name then
	  begin
	    let (ver',rev') = Hashtbl.find table pkg_name in
	    if ver <> ver' || rev <> rev' then
	      begin
		log_message
		  (sprintf "Already registered: pkg(%s) ver(%s)/rev(%d) and next found: ver(%s)/rev(%d) not equivalent." pkg_name ver' rev' ver rev);
		raise (Cannot_resolve_dependes pkg_path)
	      end
	    else (Dep_list [])
	  end
	else
	  begin
	    let new_pkg_path =
	      sprintf "%s/%s-%s-%d.%s.%s.%s" pkg_dir pkg_name ver rev
		(string_of_platform platform) arch extension in
	    Hashtbl.add table pkg_name (ver,rev);
	    Dep_list 
	      [(get_depends ~overwrite table (Dep_list []) userhost new_pkg_path); acc]
	  end)
      (System.read_lines
	~filter:(fun s -> 
	  Pcre.pmatch ~pat:"jet" s && Pcre.pmatch ~rex s)
	(sprintf "ssh %s rpm -qRp %s" userhost pkg_path))))

let rec print_depends depth = function
  | Dep_list l ->
      List.iter (fun v -> print_depends (succ depth) v) l
  | Dep_val (n,b,v,r,_) ->
      let step = String.make depth ' ' in
      printf "%s%s %s %s %d\n" step n b v r

let print_dep_val (n,b,v,r,_) =
  printf "%s %s %s %d\n" n b v r

let clone_packages l =
  List.iter (fun (n,b,v,r,_) ->
    let specdir =
      sprintf "./pack/%s/%s" n b in
    ignore (update ~specdir ~ver:(Some v) ~rev:(Some (string_of_int r)) ())) l

let rec download_packages userhost = function
  | Dep_list l ->
      List.iter (fun v -> download_packages userhost v) l
  | Dep_val (n,b,v,r,path) ->
      let src =
	sprintf "%s:%s" userhost path in
      Rules.send_file_over_ssh src "."

let list_of_depends deps =
  let rec make acc = function
    | Dep_list l -> 
	acc @ List.flatten (List.map (make []) l)
    | Dep_val  v -> acc @ [v]
  in make [] deps      

let clone userhost pkg_path mode =
  let table = Hashtbl.create 32 in
  let overwrite = mode <> "default" in
  let depends =
    get_depends ~overwrite table (Dep_list []) userhost pkg_path in

  print_endline "Depends Tree:";
  print_depends 0 depends;
  match mode with
    | "overwrite" -> clone_packages (List.rev (list_of_depends depends))
    | "depends"   ->
	print_endline "Build Order:";
	List.iter print_dep_val (List.rev (list_of_depends depends))
    | "packages"  -> download_packages userhost depends
    | _           -> clone_packages (List.rev (list_of_depends depends))

let pack_branches pkgdir =
  List.filter (fun s -> s <> "hooks.scm")
    (System.list_of_directory pkgdir)

let select_branch ~default_branch pkgdir pkg =
  match pack_branches (Filename.concat pkgdir pkg) with
    | [] -> raise (Pack_branch_is_not_found pkg)
    | hd::tl as branches ->
	(match tl with
	  | [] -> hd
	  | _ ->
	      begin
		let select () =
		  printf "Select pack-branch for %s\n%!" pkg;
		  let pack_branch_variants =
		    Array.of_list branches in
		  Array.iteri (printf "%d) %s\n%!") pack_branch_variants;
		  let n = read_number (pred (List.length branches)) in
		  pack_branch_variants.(n)
		in
		match default_branch with
		  | Some b ->
		      if List.mem b branches then b
		      else raise
			(Pack_branch_is_not_found pkg)
		  | None -> select ()
	      end)

let specdir_of_pkg ~default_branch pkgdir pkg =
  sprintf "%s/%s/%s" pkgdir pkg (select_branch ~default_branch pkgdir pkg)

let rec get_pack_depends ~default_branch table acc specdir =
  log_message (sprintf "resolve %s" specdir);
  let pkgdir =
    Filename.dirname (Filename.dirname specdir) in
  let f = Filename.concat specdir in
  let depends = make_depends ~ignore_last:true (f "depends") in
  match
    (List.fold_left
      (fun acc (pkg,_,_) ->
	if Hashtbl.mem table pkg || not (Pcre.pmatch ~pat:"jet" pkg) then
	  acc
	else
	  begin
	    Hashtbl.add table pkg false;
	    pkg::acc
	  end)
      [] depends)
  with
    | [] -> specdir::acc
    | l  ->
	(specdir::(List.flatten (List.map (fun pkg -> (get_pack_depends ~default_branch table [] (specdir_of_pkg ~default_branch pkgdir pkg))) l)))

type spectree =
  | Dval of (string * spectree)
  | Dlist of spectree list

let make_depends_tree ~default_branch table specdir : spectree =
  let pkgdir =
    Filename.dirname (Filename.dirname specdir) in
  let rec make depth specdir =
    log_message (sprintf "%s resolve %s" (String.make depth ' ') specdir);
    if Hashtbl.mem table specdir then
      begin
	log_message (sprintf "%s warning: %s already scanned" (String.make depth ' ') specdir);
	Dval (specdir, Dlist [])
      end
    else
      begin
	Hashtbl.add table specdir false;
	if Sys.file_exists specdir then
	  let depfile = Filename.concat specdir "depends" in
	  if Sys.file_exists depfile then
	    let depends =
	      List.fold_left (fun acc (pkg,_,_) ->
		try
		  acc @ [specdir_of_pkg ~default_branch pkgdir pkg]
		with _ -> acc)
		[] (make_depends ~ignore_last:true depfile)
	    in
	    Dval (specdir, Dlist (List.map (make (succ depth)) depends))
	  else
	    Dval (specdir, Dlist [])
	else 
	  Dval (specdir, Dlist [])
      end
  in make 0 specdir

type dep_path = string list

exception Found_specdir of dep_path

let upgrade specdir upgrade_mode default_branch =
  let specdir = System.path_strip_directory specdir in

  check_specdir specdir;
  check_pack ();

  let depends =
    log_message "make pack depends...";
    get_pack_depends ~default_branch (Hashtbl.create 32) [] specdir in

  let deptree =
    log_message "make spec depends...";
    make_depends_tree ~default_branch (Hashtbl.create 32) specdir in
  
  let build_table = Hashtbl.create 32 in
  let mark_table = Hashtbl.create 32 in

  let find_specdir specdir =
      let rec make acc = function
	| Dval (specdir', Dlist l) ->
	    if specdir = specdir' then
	      raise (Found_specdir (List.rev acc))
	    else List.iter (make (specdir'::acc)) l
	| Dlist l -> assert false
	| Dval (specdir', Dval _) -> assert false
      in
      try
	make [] deptree; raise (Bad_specdir specdir)
      with Found_specdir s -> s
  in

  let eval_lazy_mode specdir =
    let dep_path = find_specdir specdir in
    (* log_message ("eval lazy mode for: " ^ specdir); List.iter print_endline dep_path; *)
    if Hashtbl.mem mark_table specdir && not (Hashtbl.mem build_table specdir) then
      (false,dep_path)
    else
      (true,dep_path)
  in
  
  match upgrade_mode with
    | Upgrade_full ->
	List.iter
	  (fun specdir ->
	    ignore(update ~specdir ~lazy_mode:false ~interactive:true ()))
	  (List.rev depends)
    | Upgrade_lazy ->
	List.iter
	  (fun specdir ->
	    ignore(update ~specdir ~lazy_mode:true ~interactive:true ()))
	  (List.rev depends)
    | Upgrade_complete ->
	List.iter
	  (fun specdir ->
	    let (lazy_mode,dep_path) =
	      eval_lazy_mode specdir in
	    log_message (sprintf "lazy-mode is %b for %s, dep-path:" lazy_mode specdir);
	    List.iter log_message dep_path;
	    let updated =
	      update
		~specdir
		~lazy_mode
		~interactive:true ()
	    in
	    Hashtbl.replace build_table specdir updated;
	    if updated then
	      List.iter
		(fun s -> Hashtbl.replace mark_table s true) dep_path)
	  (List.rev depends)
	  
let branch specdir src dst =
  printf "Create new pack branch from %s to %s\n%!" src dst;

  let dir = Filename.dirname in
  let depends =
    get_pack_depends ~default_branch:(Some src) (Hashtbl.create 32) [] specdir in

  let pack_dir = dir (dir specdir) in

  let change =
    List.map
      (fun c ->
	let component_location =
	  let s = Filename.concat pack_dir c.name in
	  if Sys.file_exists s then s
	  else s ^ ".git"
	in
	let make_new_branch ?start () =
	  if not (List.mem dst (Git.git_branch ())) then
	    Git.git_create_branch ~start dst;
	  if not (List.mem (origin dst) (Git.git_branch ~remote:true ())) then
	    begin
	      Git.git_push ~refspec:dst ".";
	      Git.git_pull "origin";
	    end
	in	
	(match c.label with
	  | Current ->
	      printf "Warning: used current branch for %s component forking\n%!" c.name;
	      with_dir 
		component_location make_new_branch;
	      { c with label = Branch dst }
	  | Branch start ->
	      if c.name = "pack" then c
	      else
		begin
		  with_dir component_location (make_new_branch ~start);
		  { c with label = Branch dst }
		end
	  | Tag s ->
	      printf "Warning: tag %s unchanged while %s component forking\n%!" s c.name;
	      c))
  in
  let fork_pack_branch specdir =
    let dstdir = Filename.concat (dir specdir) dst in
    let composite = Filename.concat specdir "composite" in
    let components = 
      components_of_composite composite in
    make_directory_r dstdir;
    write_composite
      (Filename.concat dstdir composite) (change components)
  in

  let update_pack () =
    with_dir pack_dir
      (fun () ->
	Git.git_add ".";
	Git.git_commit (sprintf "add new pack branch %s from %s" dst src));
  in
  
  List.iter fork_pack_branch (List.rev depends);
  update_pack ()
  
  
  


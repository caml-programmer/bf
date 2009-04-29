open Types
open Ocs_env
open Ocs_types
open Logger
open Printf

(*** Rules execution *)

let rules_file () =
  Filename.concat (Sys.getcwd()) ".bf-rules"

let load_plugins () =
  print_string "load ac-configure...";
  Scheme.eval_code (fun _ -> print_endline "ok") "
(define-syntax ac-configure
  (syntax-rules ()
    ((_ (e1 e2) ...) (ml-ac-configure `((e1 ,e2) ...)))
    ((_ (e1 e2))     (ml-ac-configure `((e1 ,e2))))
    ((_ e1 e2)       (ml-ac-configure `((e1 ,e2))))
    ((_ (e))         (ml-ac-configure `((e ()))))
    ((_ e)           (ml-ac-configure `((e ()))))
    ((_)             (ml-ac-configure `()))))
";
  print_string "load make...";
  Scheme.eval_code (fun _ -> print_endline "ok") "
(define-syntax make
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-make `((e ()) (e1 ,e2) ...)))
    ((_ (e1 e2) ...)     (ml-make `((e1 ,e2) ...)))
    ((_ (e1 e2))         (ml-make `((e1 ,e2))))
    ((_ e1 e2)           (ml-make `((e1 ,e2))))
    ((_ (e))             (ml-make `((e ()))))
    ((_ e)               (ml-make `((e ()))))
    ((_)                 (ml-make `()))))";

  print_string "load export...";
  Scheme.eval_code (fun _ -> print_endline "ok") "
(define-syntax export
  (syntax-rules ()
    ((_ (e1 e2) ...) (ml-export `((e1 ,e2) ...)))
    ((_ (e1 e2))     (ml-export `((e1 ,e2))))
    ((_ e1 e2)       (ml-export `((e1 ,e2))))
    ((_ (e))         (ml-export `((e ()))))
    ((_ e)           (ml-export `((e ()))))
    ((_)             (ml-export `()))))";

  print_string "load update-make-params...";
  Scheme.eval_code (fun _ -> print_endline "ok") "
(define-syntax update-make-params
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-update-make-params `((e ()) (e1 ,e2) ...)))
    ((_  e  (e1 e2) ...) (ml-update-make-params `((e ()) (e1 ,e2) ...)))))";

  print_string "load user lib.scm...";
  let dir = Params.get_param "plugins-dir" in
  let plugdir =
    let start = Params.get_param "start-dir" in
    if System.is_regular (Filename.concat dir "lib.scm") then
      dir
    else
      Filename.concat start dir
  in
  if not (System.is_regular (Filename.concat plugdir "lib.scm")) then
    print_endline "skip"
  else    
    begin
      System.with_extension "scm"
	Scheme.eval_file
	(System.with_prefix plugdir (System.list_of_directory plugdir));
      print_endline "ok";
    end

let load_composite file =
  let composite = ref None in
  Env.prepare ();
  Scheme.eval_file file;
  Scheme.eval_code (fun v -> composite := Some v) "(composite)";
  match !composite with
    | None -> log_error "composite handler is not called"
    | Some v -> v
  
let components_of_composite composite =
  let composite = load_composite composite in
  let rec iter acc = function
    | Snull -> acc
    | Spair v ->
	(match v.cdr with
	  | Snull -> acc @ [Scheme.component_of_sval v.car]
	  | Spair v2 ->
	      iter (acc @ [Scheme.component_of_sval v.car]) (Spair v2)
	  | _ -> log_error "invalid composition")
    | _ -> log_error "invalid composition"
  in iter [] composite

let build_rules () =
  load_plugins ();
  Env.prepare ();
  print_string "load rules...";
  Scheme.eval_file (rules_file ());
  print_endline "ok";
  Scheme.eval_code (fun _ -> ()) "(build ())"
    
let install_rules () =
  load_plugins ();
  Env.prepare ();
  print_string "load rules...";
  Scheme.eval_file (rules_file ());
  print_endline "ok";
  if Sys.file_exists ".bf-build" then
    Scheme.eval_code (fun _ -> ()) "(install ())"
  else
    log_error "current component is not built"


(*** Component rules *)

let split_by_space s =
  Pcre.split ~pat:"\\s+" s

let add_make_opts v =
  (split_by_space (Params.get_param "make-opts")) @ v

let simple_configure args =
  log_command "./configure" args
    
let simple_make args =  
  log_command "make" (add_make_opts args)

let simple_install args =
  log_command "make" ("install"::args)

let export args =
  List.iter
    (fun (key,value) ->
      let key = String.uppercase key in
      match value with
	  Some v ->
	    Env.update key v
	| None ->
	    Env.update key "")
    args

let get_env name =
  try
    Some (Env.find name)
  with Not_found -> None

let make args =
  let rec prepare acc = function
    | [] -> List.rev acc
    | (key,value)::tl ->
	match value with
	  | Some v -> prepare ((String.uppercase key^"="^v)::acc) tl
	  | None   -> prepare (key::acc) tl
  in log_command "make" (add_make_opts (prepare [] args))

let ac_configure args =
  let have_spaces s =
    (try
      ignore(String.index s ' '); true 
    with Not_found -> false)
  in
  let rec prepare acc = function
    | [] -> List.rev acc
    | (key,value)::tl ->
	match value with
	  | Some v -> 
	      if have_spaces v then
		prepare (("--"^key^"=\""^v^"\"")::acc) tl
	      else
		prepare (("--"^key^"="^v)::acc) tl
	  | None   -> prepare (("--"^key)::acc) tl
  in log_command "./configure" (prepare [] args)

let path_concat args =
  let rec concat acc = function
      [] -> acc
    | hd::tl -> concat (Filename.concat acc hd) tl
  in concat "" args

let string_concat args =  
  let rec concat acc = function
      [] -> acc
    | hd::tl -> concat (acc ^ hd) tl
  in concat "" args

let install_file file dir =
  with_logger
    (fun logger ->
      if not (Sys.file_exists dir) then
	begin
	  log_message ~logger ("creating directory " ^ dir);
	  System.create_directory_r dir
	end;
      let name = 
	System.path_strip_directory 
	  (Filename.basename file)
      in
      let dest_file =
	Filename.concat dir name in
      if Sys.file_exists dest_file then
	Sys.remove dest_file;
      log_message ~logger ("installing " ^ file ^ " to " ^ dest_file);
      System.copy_file file dest_file)

let read_directory dir =
  let dh = Unix.opendir dir in
  let rec read acc = 
    try
      let s = Unix.readdir dh in
      if s <> "." && s <> ".." then
	read (acc @ [s])
      else
	read acc
    with End_of_file ->
      Unix.closedir dh; acc
  in read []

let with_dir dir f =
  let cur = Sys.getcwd () in
  try
    Sys.chdir dir;
    let r = f () in
    Sys.chdir cur;
    r
  with exn ->
    Sys.chdir cur; raise exn

let read_command cmd =
  System.read_lines ~env:(Env.current ()) cmd

let replace_param key value content =
  let key = String.uppercase key in
  let rex =
    Pcre.regexp 
      ~flags:[`MULTILINE] ("^" ^ key ^ "\\s*=.*?$")
  in Pcre.replace 
       ~rex 
       ~templ:(key ^ "=" ^ 
       (match value with 
	 | Some v -> v 
	 | None -> "")) content

let rec replace_params content = function
  | [] -> content
  | (key,value)::tl ->
      replace_params (replace_param key value content) tl

let update_make_params v =
  let name = fst (List.hd v) in
  let params = List.tl v in
  let tmpname = name ^ ".tmp" in
  let content =    
    System.read_file ~file:name in
  System.write_string
    ~file:tmpname ~string:(replace_params content params);
  Sys.rename tmpname name

let move_file src dir =
  let name = Filename.basename src in
  let dst = Filename.concat dir name in
  Sys.rename src dst

exception Cannot_create_directory of string

let make_directory_r ?(mode=0o755) s =
  let rec make rest s =
    let dir = Filename.dirname s in
    if System.is_directory dir then
      begin
	Unix.mkdir s mode;
	List.iter
	  (fun s -> Unix.mkdir s mode)
	  rest
      end
    else
      make (s::rest) dir
  in make [] s

let make_directory dirs =
  List.iter
    (fun dir ->
      if not (Sys.file_exists dir) then
	make_directory_r ~mode:0o755 dir
      else if System.is_directory dir then
	log_message (sprintf "warning: directory (%s) already exists!" dir)
      else
	raise (Cannot_create_directory dir))
    dirs

let move_directory src dst =
  log_command "mv" [src;dst]

let remove_directory dir =
  log_command "rm" ["-rf";dir]
  
exception Cannot_create_symlink of (string * string)

let create_symlink src dst =
  if System.is_symlink dst && Unix.readlink dst = src
  then ()
  else
    if System.is_symlink dst then
      begin
	Unix.unlink dst;
	Unix.symlink src dst
      end
    else
      if Sys.file_exists dst then
	raise (Cannot_create_symlink (src,dst))
      else
	Unix.symlink src dst

let create_link src dst =
  Unix.link src dst

let send_file_over_ssh src dst =
  log_command "scp" [src;dst]


(* Package *)

type os =
  | Linux
  | SunOS

type pkg_engine =
  | Rpm_build
  | Pkg_trans

type platform =
  | Rhel3
  | Rhel4
  | Cent4
  | Cent5
  | Fedora10
  | Alt
  | Arch
  | Solaris8
  | Solaris9
  | Solaris10

type platform_mapping =
    (string * ((string * platform) list)) list

let engine_of_platform = function
  | Rhel3     -> Rpm_build
  | Rhel4     -> Rpm_build
  | Cent4     -> Rpm_build
  | Cent5     -> Rpm_build
  | Fedora10  -> Rpm_build
  | Alt       -> Rpm_build
  | Arch      -> Rpm_build
  | Solaris8  -> Pkg_trans
  | Solaris9  -> Pkg_trans
  | Solaris10 -> Pkg_trans

let string_of_platform = function
  | Rhel3     -> "rhel3"
  | Rhel4     -> "rhel4"
  | Cent4     -> "cent4"
  | Cent5     -> "cent5"
  | Fedora10  -> "f10"
  | Alt       -> "alt"
  | Arch      -> "arch"
  | Solaris8  -> "sol8"
  | Solaris9  -> "sol9"
  | Solaris10 -> "sol10"

let platform_of_string = function
  | "rhel3" -> Rhel3
  | "rhel4" -> Rhel4
  | "cent4" -> Cent4
  | "cent5" -> Cent5
  | "f10"   -> Fedora10
  | "alt"   -> Alt
  | "arch"  -> Arch
  | "sol8"  -> Solaris8
  | "sol9"  -> Solaris9
  | "sol10" -> Solaris10
  |  s -> log_error (sprintf "Unsupported platform (%s)" s)

let os_of_string = function
  | "linux" -> Linux
  | "sunos" -> SunOS
  | s -> log_error (sprintf "Unsupported OS (%s)" s)
      
let os () =
  os_of_string (System.uname ())

let linux_platform_mapping =
  [
    "/etc/redhat-release",
    [
      "^Red Hat Enterprise.*?release 3",Rhel3;
      "^Red Hat Enterprise.*?release 4",Rhel4;
      "^CentOS.*?release 4",Cent4;
      "^CentOS.*?release 5",Cent5;
      "^Fedora.*?release 10",Fedora10;
      "^ALT Linux",Alt
    ];
    "/etc/arch-release", ["^.*",Arch]
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

let with_platform (f : os -> platform -> unit) =
  let os = os () in
  match os with
    | Linux ->
	let platform =
	  (match select_platforms [] linux_platform_mapping with
	    | [] -> log_error "unknown or unsupported platform"
	    | p::_ -> p)
	in f os platform
    | SunOS->
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
  let location = 
    match platform with
      | Cent5 ->
	  "/usr/src/redhat/RPMS/" ^ arch
      | _ ->
	  Sys.getcwd ()       
  in
  let rhsys = string_of_platform platform in
  add "-bb";
  if platform <> Cent5 then
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
      in
      log_message (sprintf "make-path %s -> %s" s m); m
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
	    let dname =
	      Filename.concat buildroot (Filename.dirname s) in
	    let src = "/" ^ s in
	    let dst = Filename.concat buildroot s in
	    log_message (sprintf "create-directory-r %s" dname);
	    System.create_directory_r dname;
	    log_message (sprintf "copy-file %s %s" src dst);
	    System.copy_file src dst
	| `Dir s ->
	    let dname =
	      Filename.concat buildroot (Filename.dirname s) in
	    let src = "/" ^ s in
	    let dst = Filename.concat buildroot s in
	    log_message (sprintf "create-directory-r %s" dname);
	    System.create_directory_r dname;
	    log_message (sprintf "remove-directory %s" dst);
	    remove_directory (Filename.concat buildroot s);
	    log_message (sprintf "copy-directory %s %s" src dst);
	    System.copy_dir src dst;
	| `Empty_dir s ->
	    let dst = Filename.concat buildroot s in
	    log_message (sprintf "create-directory-r %s" dst);
	    System.create_directory_r dst
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

type pkg_name = string
type pkg_desc = string
type pkg_ver = string
type pkg_op =
  | Pkg_le
  | Pkg_lt
  | Pkg_eq
  | Pkg_ge
  | Pkg_gt
    
type depend = 
    pkg_name * (pkg_op * pkg_ver) option * pkg_desc option

type reject =
    string (* pcre pattern *)

type spec = {
  pkgname : pkg_name;
  depends : depend list;
  rejects : reject list;
  components : component list;
  pre_install : string option;
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

let spec_from_v2 specdir =
  let f = Filename.concat specdir in
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
	  pkg_ver := Some ver;
	  pkg_op  := Some op;
	in
	
	(match name_v with
	  | Ssymbol s -> pkg_name := Some s
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
      with _ ->
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
    let n = f "depends" in
    if Sys.file_exists n then
      begin
	Scheme.parse
	  ["depends",(Scheme.iter add_os)]
	  (Ocs_read.read_from_port
	    (Ocs_port.open_input_port n));
	!acc
      end
    else []
  in
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
  let components =
    components_of_composite (f "composite") in
  let pre_install =
    load (f "pre-install") in
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
    pkgname = (Filename.basename (Filename.dirname specdir));
    depends = depends;
    rejects = rejects;
    components = components;
    pre_install = pre_install;
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

let build_over_rpmbuild params =
  let (pkgname,platform,specdir,version,release,spec,files,findreq) = params in
  let top_dir = Params.get_param "top-dir" in
  check_rh_build_env ();
  log_command "chmod" ["+x";findreq];
  copy_to_buildroot ~top_dir files;
  let (location,fullname) =
    rpmbuild
      ~top_dir 
      ~pkgname ~platform ~version ~release
      ~spec ~files ~findreq ()
  in
  let hooks =
    Filename.concat specdir "hooks.scm" in
  if Sys.file_exists hooks then
    begin
      Scheme.eval_file hooks;
      Scheme.eval_code (fun _ -> ())
	(sprintf "(after-build \"%s\" \"%s\" \"%s\")"
	  (System.hostname ()) location fullname)
    end
      
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
		    build_over_rpmbuild
		      (pkgname,platform,specdir,version,release,spec,files,findreq)
		| _-> assert false)
	  | "2.0" ->
	      let spec = spec_from_v2 abs_specdir in
	      print_depends spec.depends;
	      (match engine_of_platform platform with
		| Rpm_build ->
		    let files =
		      with_out "rpmbuild.files"
			(fun out ->
			  let bf_table = Hashtbl.create 32 in
			  let reg k =
			    if Hashtbl.mem bf_table k then "" else k in
			  let add_bf_list file =
			    let ch = open_in file in
			    let rec read () =
			      try
				let s = input_line ch in
				let l = String.length s in
				if l > 2 then
				  (match s.[0] with
				    | 'd' ->
					out (reg (sprintf "%%dir %s\n" (String.sub s 2 (l - 2))))
				    | 'f' -> 
					out (reg (sprintf "%s\n" (String.sub s 2 (l - 2))))
				    | _ -> ())
			      with End_of_file -> close_in ch
			    in read ()
			  in
			  List.iter
			    (fun c ->
			      let name = c.name in
			      let bf_list = 
				Filename.concat name ".bf-list" in
			      if Sys.file_exists bf_list then
				add_bf_list bf_list
			      else
				log_error
				  (sprintf "bf list is not found (%s)" bf_list))
			    (List.filter 
			      (fun c -> c.pkg = None)
			      spec.components))
		    in
		    let findreq =
		      with_out "rpmbuild.findreq"
			(fun out -> 
			  out "#!/bin/sh\n";
			  List.iter 
			    (fun (pkg_name,ov_opt,_) ->
			      (match ov_opt with
				| Some (op,ver) ->
				    out (sprintf "echo %s %s %s\n"
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
		    let specfile =
		      let rpm_key_format s =
			let l = String.length s in
			if l > 0 then
			  let r = String.sub s 0 l in
			  r.[0] <- Char.uppercase r.[0]; r
			else s
		      in
		      with_out "rpmbuild.spec"
			(fun out ->
			  let find_value = function
			    | "topdir" -> Params.get_param "top-dir"
			    | "name" -> spec.pkgname
			    | "version" -> version
			    | "release" -> release
			    | "buildroot" -> "buildroot"
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
			  
			  out "%define _use_internal_dependency_generator 0\n";
			  out "%define __find_requires %findreq\n";
			  out "%description\n";
			  out "%files\n";
			  out (sprintf "%%include %s\n" files);
			  
			  let resolve_params s =
			    Pcre.substitute
			      ~pat:"%\\(.*?\\)"
			      ~subst:(fun s ->
				let l = String.length s in
				let k = String.sub s 2 (l - 3) in
				(try 
				  find_value k
				with Not_found -> s))
			      s
			  in

			  let oo s = out (resolve_params s); out "\n" in
			  
			  (match spec.pre_install with
			    | None -> ()
			    | Some pre ->
				out "%pre\n";
				oo pre);
			  (match spec.post_install with
			    | None -> ()
			    | Some post ->				
				out "%post\n";
				oo post);
			  (match spec.pre_uninstall with
			    | None -> ()
			    | Some preun ->
				out "%preun\n";
				oo preun))
		    in
		    
		    build_over_rpmbuild 
		      (spec.pkgname,platform,specdir,version,release,specfile,files,findreq)
		| Pkg_trans ->
		    log_error "pkgtrans engine is not impelented")
	  | version ->
	      raise (Unsupported_specdir_version version))
    | _ -> log_error "build_rh_package: wrong arguments"

let build_package args =
  with_platform
    (fun os platfrom ->
      build_package_impl os platfrom args)

(* Log wizor *)

type log_status = {
  mtime: float;
  lname: string;
}

let log_wizor dir =
  let read_logs () =
    List.map
      (fun dir ->
	let st = Unix.stat dir in
	{ mtime = st.Unix.st_mtime; lname = dir })
    (System.list_of_directory dir)
  in ()








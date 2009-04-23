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

let make_directory dirs =
  List.iter
    (fun dir ->
      if not (Sys.file_exists dir) then
	Unix.mkdir dir 0o755
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

type platform =
  | Rhel3
  | Rhel4
  | Cent4
  | Cent5
  | Fedora10
  | Alt
  | Solaris8
  | Solaris9
  | Solaris10

type platform_mapping = 
    (string * ((string * platform) list)) list

let string_of_platform = function
  | Rhel3     -> "rhel3"
  | Rhel4     -> "rhel4"
  | Cent4     -> "cent4"
  | Cent5     -> "cent5"
  | Fedora10     -> "f10"
  | Alt       -> "alt"
  | Solaris8  -> "sol8"
  | Solaris9  -> "sol9"
  | Solaris10 -> "sol10"

let platform_mapping = [
  "/etc/redhat-release",
  [
    "^Red Hat Enterprise.*?release 3",Rhel3;
    "^Red Hat Enterprise.*?release 4",Rhel4;
    "^CentOS.*?release 4",Cent4;
    "^CentOS.*?release 5",Cent5;
    "^Fedora.*?release 10",Fedora10;
    "^ALT Linux",Alt
  ]
]

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

let check_version v file =
  let s = System.read_file file in
  try
    v = String.sub s 0 (String.index s '\n')
  with Not_found -> v = s

let specdir_format_v1 specdir =
  let flist = ["rh.spec";"rh.files";"rh.req";"version"] in
  if System.is_directory specdir &&
    List.for_all
    (fun s ->
      Sys.file_exists (Filename.concat specdir s))
    flist
  then
    if check_version "1.0" (Filename.concat specdir "version") then
      List.map 
	(if String.length specdir > 0 && specdir.[0] = '/' then
	  Filename.concat specdir
	else
	  Filename.concat (Filename.concat (Sys.getcwd()) specdir)) flist
    else
      raise Invalid_specdir_format
  else raise Invalid_specdir_format

let build_rh_package platform args =
  match args with
    | [specdir;version;release] ->
	(match specdir_format_v1 specdir with
	    [spec;files;findreq;pack_version] ->
	      let top_dir = Params.get_param "top-dir" in
	      check_rh_build_env ();
	      log_command "chmod" ["+x";findreq];
	      copy_to_buildroot ~top_dir files;
	      let (location,fullname) =
		rpmbuild
		  ~top_dir
		  ~pkgname:(Filename.basename specdir)
		  ~platform ~version ~release
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
	  | _-> assert false)
    | _ -> log_error "build_rh_package: wrong arguments"

let build_linux_package args =
  List.iter
    (fun (f,m) ->
      if Sys.file_exists f then
	let s = System.read_file ~file:f in
	let l = List.filter (fun (pat,_) -> Pcre.pmatch ~pat s) m in
	match l with
	  | (_,platform)::_ ->
	      (match platform with
		| Rhel3 -> build_rh_package platform args
		| Rhel4 -> build_rh_package platform args
		| Cent4 -> build_rh_package platform args
		| Cent5 -> build_rh_package platform args
		| Fedora10 -> build_rh_package platform args
		| Alt   -> build_rh_package platform args
		| _     -> log_error "unknown or unsupported platform")
	  | _ -> log_error "unknown or unsupported platform")
    platform_mapping

let build_sunos_package args = ()

let build_package args =
  match System.uname () with
    | "linux" -> build_linux_package args
    |  s      -> log_error (sprintf "Unsupport platform (%s) by build package" s)


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

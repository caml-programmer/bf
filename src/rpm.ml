open Printf
open Platform
open Logger
open Spectype
       
let fullname pkgname version revision rhsys arch =
  sprintf "%s-%s-%s.%s.%s.rpm" pkgname version revision rhsys arch

let fullname_dev pkgname version revision rhsys arch =
  sprintf "%s-dev-%s-%s.%s.%s.rpm" pkgname version revision rhsys arch
	  
let rpm_deps rpm_file =
  let err = Output.err "Rpm.rpm_deps" in
  let pool_dir = Path.make [(Params.get "pool-dir"); "/"] in
  let (status, str_deps, _) =
    System.with_dir pool_dir
      (fun () ->
       if not (Sys.file_exists rpm_file) then
	 err ("File does not exist: "^pool_dir^rpm_file);
       Cmd.command ~ignore_errors:true ("rpm -qRp "^rpm_file^" | grep -v '[/()]'")) in
  if status <> 0 && status <> 1 then
    err ("Command has failed with exit status \""^(string_of_int status)^
	   "\" that is not normal for grep");
  let dep_of_str depstr =
    match Output.string_list_of_string ~separator:" " depstr with
    | [pkgname; op_str; release] ->
       let opver = if op_str = "" then
		     None
		   else let op = op_of_string op_str in
			Some (op, release) in
       ((pkgname, opver, None) : platform_depend)
    | [pkgname] -> ((pkgname, None, None) : platform_depend)
    | _ -> err ("wrong dep: "^depstr) in
  List.map dep_of_str (Output.string_list_of_string str_deps)

let list_pkgs_in_dir dir =
  let dh = Unix.opendir dir in
  let is_pkg = Strings.have_suffix ".rpm" in
  let rec look_for_pkg acc =
    try let file = Unix.readdir dh in
	look_for_pkg (if is_pkg file then
			file :: acc
		      else acc)
    with End_of_file -> Unix.closedir dh; acc in
  look_for_pkg []

(*  let err = Output.err "Rpm.parse_fullname" in *)

let parse_fullname str =
  let err = failwith in
  let rx = Str.regexp "\\(.*\\)-\\(.*\\)-\\(.*\\)\\.\\(.*\\)\\.\\(.*\\)\\.rpm" in
  try ignore (Str.search_forward rx str 0);
      let pkgname = Str.matched_group 1 str in
      let version = Str.matched_group 2 str in
      let revision = Str.matched_group 3 str in
      let rhsys = Str.matched_group 4 str in
      let arch = Str.matched_group 5 str in
      (pkgname, version, revision, rhsys, arch)
  with Not_found -> err ("Wrong rpm filename format: "^str)

(* Резолвит локальные зависимости по пакетам в pool-е *)
let resolve_ldeps ?(os=Platform.os()) ?(platform=Platform.current()) (deplist:platform_depend list) =
  let err = Output.err "Rpm.resolve_ldeps" in
  let deperr dep = err ("Can't resolve dependency: "^(string_of_platform_depend dep)) in
  
  let is_local_dep (pkgname,_,_) = Pkg.is_local pkgname in

  let pool_dir = Path.make [(Params.get "pool-dir"); "/"] in
  let pkgs = list_pkgs_in_dir pool_dir in
  let pkgs = List.map parse_fullname pkgs in
  let pkgs = List.filter (fun (pkgname,version,revision,rhsys,arch) ->
			  rhsys = (string_of_platform platform))
			 pkgs in
  let pkgs_table = Hashtbl.create 32 in
  List.iter
    (fun (pkgname,version,revision,rhsys,arch) ->
     if not (Hashtbl.mem pkgs_table pkgname) then
       Hashtbl.add pkgs_table pkgname (Hashtbl.create 32);
     let ver_table = Hashtbl.find pkgs_table pkgname in
     if not (Hashtbl.mem ver_table version) then
       Hashtbl.add ver_table version (Hashtbl.create 32);
     let rev_table = Hashtbl.find ver_table version in
     Hashtbl.add rev_table (int_of_string revision) (fullname pkgname version revision rhsys arch))
    pkgs;

  let keys table =
    Hashtbl.fold (fun key _ acc -> key :: acc) table [] in
  let available_versions pkgname =
    keys (Hashtbl.find pkgs_table pkgname) in
  let available_revisions pkgname version =
    let revs = keys (Hashtbl.find (Hashtbl.find pkgs_table pkgname) version) in
    List.iter (fun rev -> print_endline ("FFF: "^pkgname^" "^version^" "^(string_of_int rev))) revs;
    revs in
  let find_pkg pkgname version revision =
    Hashtbl.find (Hashtbl.find (Hashtbl.find pkgs_table pkgname) version) revision in

  let maxl_int int_list = List.fold_left max 0 int_list in
  let maxl_ver ver_list = List.fold_left Version.max "0" ver_list in
  
  let deptable = Hashtbl.create 5 in
  let depadd pkg = Hashtbl.add deptable pkg true in
  let stored_deps () = keys deptable in
  
  let rec resolve dep =
    let (pkgname, oprel, _) = dep in
    let rpm_file =
      try
	match oprel with
	| None -> err "Programmer wasn't able to imagine the situation when opver option is None :)"
	| Some (op, release) ->
	   let (ver, rev) = Version.ver_rev_of_release release in
	   match op with
	   (* требование релиза соответствовать версии и ревизии *)
	   | Pkg_last
	   | Pkg_eq -> let rev = match rev with
			 | "" -> maxl_int (available_revisions pkgname ver)
			 | _ as rev -> (int_of_string rev) in
		       find_pkg pkgname ver rev
	   (* версия может быть и больше заданной *)
	   | Pkg_ge -> let maxver = maxl_ver (available_versions pkgname) in
		       if Version.less maxver ver then raise Not_found;
		       let maxrev = maxl_int (available_revisions pkgname maxver) in
		       (match rev with
			| "" -> ()
			| _ as rev -> let rev = int_of_string rev in
				      if (Version.equal maxver ver) && (maxrev < rev)
				      then raise Not_found);
		       find_pkg pkgname maxver maxrev
	   | Pkg_gt -> let maxver = maxl_ver (available_versions pkgname) in
		       if Version.greater ver maxver then raise Not_found;
		       let maxrev = maxl_int (available_revisions pkgname maxver) in
		       (match rev with
			| "" -> ()
			| _ as rev -> let rev = int_of_string rev in
				      if rev > maxrev then raise Not_found);
		       find_pkg pkgname maxver maxrev
	   (* когда встречаются другие зависимости, и как с ними обращаться, пока не ясно *)
	   | _ -> err ("Unsupported rpm op: "^(string_of_op op))
      with Not_found -> deperr dep in
    depadd rpm_file;
    List.iter resolve (List.filter is_local_dep (rpm_deps rpm_file)) in

  List.iter resolve (List.filter is_local_dep deplist);
  stored_deps ()

(* Создаёт дубликат строки и делает её первый символ заглавным *)
let key_format s =
  let l = String.length s in
  if l > 0 then
    let r = String.sub s 0 l in
    r.[0] <- Char.uppercase r.[0]; r
  else s

let build
  ?(top_label="topdir") ?(top_dir="/tmp/rpmbuild")
  ?(nocopy="/") ?(buildroot=(Filename.concat (Sys.getcwd ()) "buildroot"))
  ?(format="%%{NAME}-%%{VERSION}-%%{RELEASE}.%%{ARCH}.rpm")
  ?chroot
  ~pkgname ~platform ~version ~release ~spec ~files ~findreq () =
  print_endline ("FCK: topdir = "^top_dir);
  let args = ref [] in
  let add s = args := !args @ [s] in
  let define n v =
    print_endline ("define: "^n^" = "^v);
    add "--define"; add (sprintf "%s %s" n v)
  in
  let arch = System.arch () in
  let location = Sys.getcwd () in
  let rhsys = string_of_platform platform in
  add "-bb";
  add ("--target=" ^ arch);
  add spec;
  let as_root =
    match chroot with
    | None -> define "buildroot" buildroot;
	      false
    | Some chroot -> define "buildroot" "/buildroot";
		     add "--root"; add chroot;
		     true in
  define "_rpmdir" location;
  define "fileslist" files; (* must be absolute *)
  define top_label top_dir;
  define "nocopy" nocopy;
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
  
  let fullname = fullname pkgname version release rhsys arch in
  if Sys.file_exists (Filename.concat location fullname) then
    (* Чёрт возьми, если файл существует, не предпринимается попытки его пересобрать! *)
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
	  (*let _ =
	    Unix.execvp "rpmbuild" (Array.of_list ("rpmbuild"::!args)) in*)
	  let command = Cmd.choose_command ~as_root ~loglevel:"always" () in
	  ignore (command cmd);
	  exit 0
	end
    end

let copy_to_buildroot ?(buildroot=(Filename.concat (Sys.getcwd ()) "buildroot")) ~top_dir files =
  
  Commands.remove_directory buildroot;
  Commands.make_directory [buildroot];
  
  let match_prefix p v =
    let pl = String.length p in
    let vl = String.length v in
    vl >= pl && String.sub v 0 pl = p
  in
  let parse_line s =
    let make_path s =
      let n =
	  Strings.substring_replace ("%dir ","")
	  (Strings.substring_replace ("%topdir",top_dir)
	    (Strings.substring_replace ("%config\\(noreplace\\) %topdir",top_dir) s)) in
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
		match Params.dest_dir () with
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
		match Params.dest_dir () with 
		  | Some d -> Filename.concat d (System.strip_root s)
		  | None   -> s in
	      let dname =
		Filename.concat buildroot (Filename.dirname s) in
	      let src = "/" ^ ns in
	      let dst = Filename.concat buildroot s in
	      System.create_directory_r dname;
	      Commands.remove_directory (Filename.concat buildroot s);
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

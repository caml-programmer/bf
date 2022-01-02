open Output
open Platform
open Printf
open Component
open Spectype
open Ocs_types
open Ext

type mount_point = {
  src : string;
  dst : string;
  options : string list;
  fstype : string option;
}

(* по факту это спек для chroot-окружений *)
type chroot = {
  name : string;
  mutable path : string;
  mount : mount_point list;
}

let mount_point_cmd chroot point =
  let opts = match point.options with
    | [] -> ""
    | _ as opts -> "-o " ^ (String.concat "," opts) in
  let fstype = match point.fstype with
    | None -> ""
    | Some t -> "-t " ^ t in
  let src = point.src in
  let dst = chroot.path ^ point.dst in
  join_with_spaces ["mount"; fstype; opts; src; dst]

let umount_point_cmd chroot point =
  let dst = chroot.path ^ point.dst in
  join_with_spaces ["umount"; dst]

let mount_all_points chroot =
  List.iter (fun point -> ignore (Cmd.root_command ~loglevel:"low" (mount_point_cmd chroot point)))
    chroot.mount

let umount_all_points chroot =
  List.iter (fun point -> ignore (Cmd.root_command ~loglevel:"low" (umount_point_cmd chroot point)))
    chroot.mount

let string_of_chroot chroot_spec =
  string_of_string_list
    [("Chroot name: "^chroot_spec.name);
    ("Chroot path: "^chroot_spec.path);
    "Chroot mounts:";
    (prefix_textblock "  "
      (string_of_string_list
	(List.map (mount_point_cmd chroot_spec)
	  chroot_spec.mount)))]

let find_mount_supply supply =
  let supplies_dir = Params.get_param "mount-supplies-path" in
  let supply = (Path.expand_globs supply) in
  let supply = if supply.[0] == '/' then supply
  else Path.make [(Path.expand_globs supplies_dir); supply] in
  if not (Sys.file_exists supply) then
    err "find_mount_supply" ("Mount supply not found: " ^ supply);
  supply

(* если src отсутствует, значит "none",
 * dst -- это путь относительно chroot-окружения, если не абсолютный
 * если options отсутствуют, значит [],
 * если type отсутствует, значит None
 *)
let load_chroot_cfg chroot_name platform =
  let err = Output.err "load_chroot_cfg" in
  let platform = Platform.string_of_platform platform in
  let read_string_value sexp errmsg =
    match sexp with
      | Sstring x -> Bytes.to_string x
      | _ as x -> Scheme.print x; err errmsg in
  System.with_dir (Params.get_param "chroot-spec")
    (fun () ->
      if Sys.file_exists chroot_name
      then let fullcfg = Scm.read_record (Scm.read_file chroot_name) in
      let cfg = Scm.read_record (List.assoc platform fullcfg) in
      let path = match List.assoc "path" cfg with
	| Sstring p -> Bytes.to_string p
	| _ as p -> Scheme.print p; err "Bad path" in
      let mpoints_sexp = try List.assoc "mount" cfg with
	| Not_found -> Snull in
      let mpoint_sexp_list = Scm.read_list mpoints_sexp in
      let mpoints =
	List.map
	  (fun p ->
	    let p = Scm.read_record p in
	    let src = try read_string_value (List.assoc "src" p) "Bad source"
	    with Not_found -> "none" in
	    let src = if src = "none" then src else find_mount_supply src in
	    let dst = try read_string_value (List.assoc "dst" p) "Bad destination"
	    with Not_found -> err "destination not found" in
	    let fstype = try Some (read_string_value (List.assoc "type" p) "Bad fstype")
	    with Not_found -> None in
	    let opts = try List.map Scm.make_string (Scm.read_list (List.assoc "opts" p))
	    with Not_found -> [] in
	    { src = (Path.expand_globs src);
	    dst = (Path.expand_globs dst);
	    options = opts;
	    fstype = fstype; })
	  mpoint_sexp_list in
      {
	name = chroot_name;
	path = Path.expand_globs path;
	mount = mpoints;
      }
      else err ("Can't find chroot spec for: "^chroot_name))

exception No_chroot_command

let chroot_cmd_exists =
  let chroot = Params.get "chroot-path" in
  Sys.file_exists chroot

let centos7_base_repo_filename = "Centos7-Base.repo"
let centos_base_repo_content () =
  let centos_mirror = Params.get_param "centos-mirror" in
  "[Centos$releasever-Base]
name=Base Repo for Centos$releasever - $basearch
baseurl="^centos_mirror^"$releasever/os/$basearch/
gpgcheck=1
gpgkey="^centos_mirror^"$releasever/os/$basearch/RPM-GPG-KEY-CentOS-$releasever"

let compose_home_path () =
  let err msg = err "Chroot.compose_home_path" msg in
  try Sys.getenv "HOME" with Not_found -> err "Cannot determine user's home directory"

let chroots_dir () =
  let chroots_dir = Params.get "chroots-dir" in
  Output.msg "chroots_dir" "high" chroots_dir;
  chroots_dir
    
(* если chroot_name начинается на /, то он и возвращается, считаясь
   абсолютным путём к chroot-окружению. В противном случае
   возвращается $home/$chroots-dir/$chroot_name *)
let compose_chroot_path chroot_name =
  if Path.is_absolute chroot_name
  then chroot_name
  else Path.make [(chroots_dir ()); chroot_name]

let projects_path () =
  Path.make ["/"; Params.get_param "projects-dir"]
    
let compose_projects_path chroot_name =
  Path.make [(compose_chroot_path chroot_name); (projects_path ())]

let chroot_shell chroot_path shell =
  Unix.execvp "chroot" [|"chroot"; chroot_path; shell|]
    
let command ?(loglevel="always") chroot_name command =
  if not chroot_cmd_exists then raise No_chroot_command;
  let chroot_cmd_path = Params.get "chroot-path" in
  let chroot_dir_path = compose_chroot_path chroot_name in
  let chroot_command = chroot_cmd_path^" "^chroot_dir_path^" "^command in
  Cmd.root_command ~loglevel chroot_command
    
(* Эта функция создаёт chroot-окружение с именем chroot_name в
   директории, описываемой параметром chroots-dir. В этом окружении
   разворачивается базовая система platform *)
let make chroot_name platform =
  let msg = msg "Chroot.make" in
  let err msg = err "Chroot.make" msg in
  let unsupported_platform platform = err ("Unsupported platform " ^ (string_of_platform platform)) in
  let root_command = Cmd.root_command ~loglevel:"high" in
  
  Cmd.mkdir_if_not_exists ~as_root:true (chroots_dir ());
  let chroot_dir = compose_chroot_path chroot_name in
  Cmd.rmdir_if_exists ~as_root:true ~recursive:true chroot_dir;
  Cmd.mkdir ~as_root:true chroot_dir;
  
  match platform with
    | Cent7 ->
	msg "always" ("Creating chroot environment for cent7 in "^chroot_dir);
	let centos_mirror = Params.get_param "centos7-mirror" in
	let centos_release_pkg_url = Repo.centos_release_pkg_url centos_mirror in
	let centos_release_pkg = Repo.pkg_by_url centos_release_pkg_url in
	let centos_release_pkg_path = Path.make [(chroots_dir ());centos_release_pkg] in

	(* разворачиваем базовое окружение *)
	ignore (root_command ("wget "^centos_release_pkg_url^" -O "^centos_release_pkg_path));
	ignore (root_command ("rpm --root "^chroot_dir^" --initdb"));
	ignore (root_command ("rpm --root "^chroot_dir^" --nodeps -ivh "^centos_release_pkg_path));

	(* заменяем репы на нужные *)
	ignore (root_command ("rm "^(Path.make [chroot_dir; "/etc/yum.repos.d/*.repo"])));
	ignore (root_command ("cp /opt/dozor/bf/data/centos7-repos/*.repo "^
	(Path.make [chroot_dir; "/etc/yum.repos.d/"])));
	(* ставим yum *)
	ignore (Cmd.root_command ~loglevel:"low" ("yum --installroot="^chroot_dir^" install yum -y"));
	
	(* создаём в chroot-е каталог для проектов *)
	ignore (root_command ("mkdir -p "^(Path.make [chroot_dir; "projects/"])));

	(* на этом пока фантазия останавливается *)
	msg "always" "Chroot has been created successfully!";
	()
    | p -> unsupported_platform p

(* копирование уже готового chroot-окружения в новую директорию для сборки пакета *)
let copy chroot_name new_chroot_name =
  Output.msg "Chroot.copy" "low" ("Copy chroot '"^chroot_name^"' to '"^new_chroot_name^"'");
  let chroot_path = compose_chroot_path chroot_name in
  let new_chroot_path = compose_chroot_path new_chroot_name in
  ignore (Cmd.root_command ~loglevel:"high" ("cp -r \""^chroot_path^"\" \""^new_chroot_path^"\""))

let delete_if_exists chroot_name =
  Output.msg "Chroot.delete_if_exists" "low" ("Delete chroot '"^chroot_name^"'");
  let chroot_path = compose_chroot_path chroot_name in
  Cmd.rmdir_if_exists ~as_root:true ~recursive:true chroot_path
    
(* Эта функция копирует компонент в директорию $chroot_name/projects *)
let clone_component ?(as_root=true) chroot_name component =
  Component.checkout_new component;
  let component_path = "file://"^(Path.make_absolute (Component.path component)) in
  System.with_dir (compose_projects_path chroot_name)
    (fun () -> Component.clone_last ~as_root ~from:component_path component)

let build_component chroot_name component_name rules =
  let msg = Output.msg "Chroot.buid_component" in
  let err = Output.err "Chroot.buid_component" in
  let chroot_path = compose_chroot_path chroot_name in
  let projects_relative_path = projects_path () in
  let project_path = Path.make [projects_relative_path; component_name] in

  msg "always" "BUILD COMPONENT";
  
  msg "always" ("chroot-path: "^chroot_path);
  msg "always" ("proj_rel_path: "^projects_relative_path);
  msg "always" ("project_path:  "^project_path);
  
  (* в chroot-окружении установка происходит непосредственно в систему *)
  Params.set "dest-dir" "";
  
  if not Cmd.i_am_root then err "Need to be run under root to make chroot call";
  Unix.chdir chroot_path;
  Unix.chroot chroot_path;

  msg "always" ("Chroot to dir: "^chroot_path);

  (* часть нижеследующего кода является копипастой из модуля component *)
  System.with_dir project_path
    (fun () ->
      (* для сборки -dev пакетов *)
      let dev_dir_orig = Params.get "dev-dir" in
      let dev_dir = "/opt/dev" in (* пусть внутри chroot-окружений она всегда будет такой *)
      if dev_dir = "/" then err "dev-dir can't be /"; (* залог на будущее *)

      Params.update_param "dev-dir" dev_dir;
      Cmd.mkdir_if_not_exists dev_dir;

      let top_dir = Params.get_param "top-dir" in
      let dest_dir = Params.get_param "dest-dir" in
      let install_dir = Params.make_install_dir () in

      Params.update_param "orig-top-dir" top_dir;
      Cmd.mkdir_if_not_exists top_dir;
      Params.update_param "install-dir" install_dir;
      Cmd.mkdir_if_not_exists install_dir;

      print_endline ("TOP-DIR: "^top_dir);
      print_endline ("DEV-DIR: "^dev_dir);
      print_endline ("INS-DIR: "^install_dir);
      print_endline ("PROJECT: "^projects_relative_path);
      
      (* build *)
      msg "always" ("switch to component path: "^project_path);
      let start_time = Unix.gettimeofday () in
      msg "always" ("build "^component_name^" started");
      Rules.build_rules rules;
      msg "always" ("build "^component_name^" has been finished");
      Rules.fill_bf_build ?rules start_time;

      (* install *)
      msg "always" ("install "^component_name^" started");
      let start_time = Unix.gettimeofday () in

      
      let oldstate = Chroot_scanner.filter
	(fun path -> not (String.have_prefix projects_relative_path path))
	(Chroot_scanner.scan "/") in

      if dest_dir <> "" then 
	Params.update_param "top-dir" install_dir; (* Deprecated: use install-dir *)
      Rules.install_rules rules;

      let newstate = Chroot_scanner.filter
	(fun path -> not (String.have_prefix projects_relative_path path))
	(Chroot_scanner.scan "/") in

      Params.update_param "top-dir" top_dir;
      Params.update_param "dev-dir" dev_dir_orig;


      let changes = Chroot_scanner.changes oldstate newstate in

      (*     print_endline "\nOLDSTATE\n";
	     List.iter (fun file -> if String.have_prefix "/opt/dev" file then
	     print_endline file)
	     (Chroot_scanner.files oldstate);

	     print_endline "\nNEWSTATE\n";
	     List.iter (fun file -> if String.have_prefix "/opt/dev" file then
	     print_endline file)
	     (Chroot_scanner.files newstate); *)
      
      print_endline "\nCHANGES\n";
      List.iter print_endline (Chroot_scanner.files changes);
      
      let files_dev = Chroot_scanner.filter (String.have_prefix dev_dir) changes in
      let files_top =
	Chroot_scanner.filter
	  (String.have_prefix install_dir)
	  (Chroot_scanner.filter (fun path -> not (String.have_prefix dev_dir path)) changes) in

      let list_file = Rules.list_file rules in
      let devlist_file = Rules.devlist_file rules in

      System.with_out list_file
	(fun ch -> output_string ch ((Chroot_scanner.gen_bflist_content files_top)^"\n"));
      System.with_out devlist_file
	(fun ch -> output_string ch ((Chroot_scanner.gen_bflist_content files_dev)^"\n"));

      msg "always" ("install "^component_name^" has been finished");
      Rules.fill_bf_install ?rules start_time
    )

let depinstall ?(os=Platform.os ()) ?(platform=Platform.current ()) chroot_name pkgspec =
  let pkgs = List.map Spectype.pkgname_of_platform_depend pkgspec.builddeps in
  let local_pkgs = List.filter Pkg.is_local pkgs in
  let system_pkgs = List.filter (fun pkg -> not (List.mem pkg local_pkgs)) pkgs in
  let chroot_dir = compose_chroot_path chroot_name in
  let err = Output.err "Chroot.depinstall" in
  
  (* копируем локальные пакеты из пула в chroot, и устанавливаем их там*)
  if List.length local_pkgs <> 0 then
    begin
      let pool_dir = Path.make [(Params.get "pool-dir"); "/"] in
      let local_pkgs = Rpm.resolve_ldeps ~os ~platform pkgspec.builddeps in
      let full_pkgs = List.remove_duplicates
	(List.map (fun pkg -> Path.make [pool_dir; pkg]) local_pkgs) in
      let full_pkgs_str = Output.string_of_string_list ~separator:" " full_pkgs in
      ignore (Cmd.root_command ~loglevel:"always" ("cp "^full_pkgs_str^" "^chroot_dir^"/"));
      let pkgs = List.remove_duplicates
	(List.map (fun pkg -> Path.make ["/"; pkg]) local_pkgs) in
      let pkgs_str = Output.string_of_string_list ~separator:" " pkgs in
      let localinstall_command = match engine_of_platform platform with
	| Rpm_build -> "yum localinstall -y "^pkgs_str
	| _ -> err ("Don't know how to install LOCAL package on platform "^(string_of_platform platform)) in
      ignore (command ~loglevel:"high" chroot_name localinstall_command)
    end;

  (* устанавливаем системные пакеты *)
  if List.length system_pkgs <> 0 then
    begin
      let pkgs_str = Output.string_of_string_list ~separator:" " system_pkgs in
      let install_command = match engine_of_platform platform with
	| Rpm_build -> "yum install -y "^pkgs_str
	| Deb_pkg -> "apt-get install -y "^pkgs_str
	| _ -> err ("Don't know how to install package on platform "^(string_of_platform platform)) in
      ignore (command ~loglevel:"high" chroot_name install_command)
    end

(* всё, что касается генерации rpm *)

let copy_to_buildroot chroot_name file_rpmbuild_files buildroot =
  let msg = Output.msg "Chroot.copy_to_buildroot" in
  let err = Output.err "Chroot.copy_to_buildroot" in
  let chroot_path = compose_chroot_path chroot_name in
  let top_dir = Params.get_param "top-dir" in

  if not Cmd.i_am_root then err "Need to be run under root to make chroot call";
  System.copy_file file_rpmbuild_files chroot_path;
  msg "always" ("Chroot to "^chroot_path);
  Unix.chdir chroot_path;
  Unix.chroot chroot_path;

  msg "always" ("Copy files to buildroot: "^buildroot);
  Rpm.copy_to_buildroot ~buildroot ~top_dir
    (Path.make ["/"; (Filename.basename file_rpmbuild_files)])
    
let make_rpmbuild_files bf_files =
  let content =
    Hashtbl.fold
      (fun fname ftype acc ->
	let str = match ftype with
	  | 'd' -> "%dir "^fname
	  | 'f' -> fname
	  | c -> failwith ("Strange type symbol in .bf-list string: " ^ (Strings.string_of_char c)) in
	str :: acc)
      bf_files [] in
  Output.string_of_string_list content

let make_rpm_findreq_line ((pkgname, opver_opt, _) : platform_depend) =
  match opver_opt with
    | None -> "echo "^pkgname
    | Some (op,ver) -> "echo \""^pkgname^" "^(string_of_pkg_op op)^" "^ver^"\""

let make_reject_template rejects =
  Output.string_of_string_list ~separator:"|" rejects

let compose_pkgpack_dir pkgname =
  let pkgpack_dir = Params.get "pkg-pack-dir" in
  Cmd.mkdir_if_not_exists pkgpack_dir;
  let dirname = Path.make [pkgpack_dir; pkgname] in
  Cmd.mkdir_if_not_exists dirname;
  dirname

let rec pack_rpm ?(devf=false) ?(os=Platform.os ()) ?(platform=Platform.current ())
  chroot_name pkgspec =
  let msg = Output.msg "Chroot.pack_rpm" in
  let pkgname = pkgspec.pkgname ^ (if devf then "-dev" else "")in
  let version = pkgspec.version in
  let revision = pkgspec.revision in
  let nodev = pkgspec.nodev in
  let ver_release = version^"-"^(string_of_int revision)^"."^(string_of_platform platform) in
  
  let chroot_path = compose_chroot_path chroot_name in
  let projects_relative_path = projects_path () in
  let projects_path = Path.make [chroot_path; projects_relative_path] in

  let pack_param = Params.get "pack" in
  
  let bf_files = Hashtbl.create 32 in
  let reg file ftype =
    let edit_param = match Hashtbl.mem bf_files file with
      | true -> Hashtbl.replace
      | false -> Hashtbl.add in
    edit_param bf_files file ftype in
  let filecount () = Hashtbl.length bf_files in

  let components = List.filter (fun c -> c.pkg = None && (not c.nopack)) pkgspec.components in
  let components = List.filter (fun (comp:component) -> comp.name <> (Filename.basename pack_param))
    components in
  
  let pkgpack_dir = compose_pkgpack_dir pkgname in

  let file_rpmbuild_files = Path.make [pkgpack_dir; "rpmbuild.files"] in
  let file_rpmbuild_findreq = Path.make [pkgpack_dir; "rpmbuild.findreq"] in
  let file_rpmbuild_spec = Path.make [pkgpack_dir; "rpmbuild.spec"] in
  let buildroot = "buildroot" in
  
  (* В текущей реализации поле custom_pkg_files никак не обрабатывается
   * Какова была его изначальная задумка, мне не понятно.
   * Возможно, какие-то файлы пакета, которые заполняются при помощи хуков. *)
  
  let _ (* custom_pkg_files *) =
    Pkgbuild.call_before_build
      ~snapshot:false ~pkgname:pkgname
      ~version ~revision:(string_of_int revision)
      ~platform None in
  
  (* генерируем список файлов *)

  msg "always" ("Generating file "^file_rpmbuild_files);
  List.iter
    (fun (component : Component.component) ->
      System.with_dir (Path.make [projects_path; component.name])
      (fun () ->
	let reg_file bf_list_file =
	  let bf_list = System.list_of_file bf_list_file in
	  List.iter (fun str ->
	    if str <> "" then
	      let ftype = str.[0] in
	      let fname = String.sub str 2 ((String.length str) - 2) in
	      reg fname ftype)
	    bf_list in
	if nodev then (* если nodev, то всё пихаем dev-dir в базовый пакет *)
	  begin
	    reg_file (Rules.list_file component.rules);
	    reg_file (Rules.devlist_file component.rules)
	  end
	else
	  if devf then
	    reg_file (Rules.devlist_file component.rules)
	  else
	    reg_file (Rules.list_file component.rules)
      ))
    components;
  let rpmbuild_files = make_rpmbuild_files bf_files in
  System.with_out file_rpmbuild_files
    (fun ch -> Output.print_endline_to_channel ch rpmbuild_files);

  (* генерируем findreq -- скрипт, определяющий в rpmbuild список зависимостей *)

  msg "always" ("Generating file "^file_rpmbuild_findreq);
  System.with_out file_rpmbuild_findreq
    (fun ch ->
      let print_endline = Output.print_endline_to_channel ch in
      let prin = Output.print_to_channel ch in
      print_endline "#!/bin/sh";

      (* нужна для того, чтобы в last-зависимостях ревизию править расставлялись *)
      let hack_last_dep (dep : platform_depend) =
	let (pkg, opver_opt, desc_opt) = dep in
	match opver_opt with
	  | None -> dep
	  | Some (op, ver) ->
	      match op with
		| Pkg_last -> (* Определяем ревизию *)
		    let maxrev = Ptag.find_max_rev pkg ver in
		    let newrev = string_of_int (succ maxrev) in
		    let platform = string_of_platform platform in
		    (pkg, Some (Pkg_eq, (ver^"-"^newrev^"."^platform)), desc_opt)
		| _ -> dep
      in

      let depends = if devf then
	(* dev-пакет должен зависеть от bin-пакета *)
	(pkgspec.pkgname, Some(Pkg_eq, ver_release), None)
	:: (List.map hack_last_dep pkgspec.devdeps)
      else (List.map hack_last_dep pkgspec.depends) in

      print_endline (Output.string_of_string_list
	(List.map make_rpm_findreq_line depends));
      let find_requires = "/usr/lib/rpm/find-requires" in
      if Sys.file_exists find_requires then
	begin
	  prin (find_requires ^ " | grep -v ^$ ");
	  let reject_tmpl = make_reject_template pkgspec.rejects in
	  if reject_tmpl <> "" then
	    prin ("| grep -v '"^reject_tmpl^"'")
	end
      else msg "always" (find_requires^" is not found!"));

  (* генерируем spec-файл для rpmbuild *)

  msg "always" ("Generating file "^file_rpmbuild_spec);
  let provides_with_arch libraries = (* здесь можно указать тип библиотек*)
    if System.arch () = "x86_64" then
      List.map (fun p -> if Pcre.pmatch ~rex:(Pcre.regexp "^lib") p then p ^ "()(64bit)" else p)
	libraries
    else libraries in
  let top_dir = Params.get_param "top-dir" in
  let find_value = function
    | "topdir" -> if nodev then top_dir else
	if devf then "/opt/dev"
	else top_dir
    | "prefix" -> if nodev then top_dir else
	if devf then "/opt/dev"
	else top_dir
    | "name" -> pkgname
    | "version" -> version
    | "release" | "revision"  -> (string_of_int revision) ^ "." ^ (string_of_platform platform)
    | "buildroot" -> buildroot
    | "provides" ->
	String.concat ", " (provides_with_arch pkgspec.provides)
    | "obsoletes" ->
	String.concat ", " pkgspec.obsoletes
    | k -> Hashtbl.find pkgspec.params k in

  System.with_out file_rpmbuild_spec
    (fun ch ->
      let print_endline = Output.print_endline_to_channel ch in
      let gen_section param =
	try print_endline (sprintf "%s: %s" (Rpm.key_format param) (find_value param))
	with Not_found -> () in
      gen_section "summary";
      gen_section "name";
      gen_section "version";
      gen_section "release";
      gen_section "license";
      gen_section "vendor";
      gen_section "group";
      gen_section "url";
      gen_section "buildroot";
      gen_section "prefix";
      if pkgspec.provides <> [] then gen_section "provides";
      if pkgspec.obsoletes <> [] then gen_section "obsoletes";
      
      print_endline "%define _use_internal_dependency_generator 0";
      print_endline "%define __find_requires %findreq";
      print_endline "%description";
      
      print_endline "%files";
      if filecount () > 0 then print_endline ("%%include "^file_rpmbuild_files);

      let resolve_and_print_endline s = print_endline (Pkgbuild.resolve_params find_value s) in
      
      (match pkgspec.pre_install with
	| None -> ()
	| Some pre ->
	    print_endline "%pre";
	    print_endline "if [ \"$1\" = \"1\" ] ; then # first install";
	    print_endline "echo -n";
	    resolve_and_print_endline pre;
	    print_endline "fi";
	    (match pkgspec.pre_update with
	      | None -> ()
	      | Some preup ->
		  print_endline "if [ \"$1\" = \"2\" ] ; then # update";
		  print_endline "echo -n";
		  resolve_and_print_endline preup;
		  print_endline "fi"));
      (match pkgspec.post_install with
	| None -> ()
	| Some post ->
	    print_endline "%post" (*-p /bin/bash*);
	    (*let files = Hashtbl.keys bf_files in*)
	    (*let qfiles = List.map Output.surrount_dquotes files in*)
	    (*let qfiles_str = Output.string_of_string_list qfiles in*)
	    print_endline ("BF_VERSION=2");
	    (*print_endline ("FILES=("^qfiles_str^")");*)
	    resolve_and_print_endline post);
      (match pkgspec.pre_uninstall with
	| None -> ()
	| Some preun ->
	    print_endline "%preun";
	    print_endline "if [ \"$1\" = \"0\" ] ; then # all versions deleted";
	    print_endline "echo -n";
	    resolve_and_print_endline preun;
	    print_endline "fi")
    );

  (* Копируем в $chroot/buildroot файлы *)
  ignore (command ~loglevel:"always" chroot_name
    "sh -c 'test -d /buildroot && rm -rf /buildroot || true'");
  ignore (Cmd.root_command ~loglevel:"always"
    ("/bin/bf copy-to-buildroot "^chroot_name^" "^file_rpmbuild_files^" "^buildroot));

  (* Запускаем сборку пакета! *)
  msg "always" "Start pack process";
  Pkgbuild.build_over_rpmbuild
    ~chroot:chroot_path
    ~snapshot:false (pkgname,platform,version,(string_of_int revision),
    file_rpmbuild_spec,file_rpmbuild_files,file_rpmbuild_findreq,
    None);
  (* Последний параметр -- hooks. Устарел. *)

  (* устанавливаем правильные права на файл *)
  let user = Unix.getlogin () in
  let pkgfile = Rpm.fullname pkgname version (string_of_int revision)
    (string_of_platform platform) (System.arch ()) in
  ignore (Cmd.root_command ~loglevel:"always"
    ("chown "^user^" "^pkgfile));

  (* перемещаем новые собранные пакеты в pool *)
  let pool = Params.get "pool-dir" in
  Cmd.mkdir_if_not_exists pool;
  ignore (Cmd.root_command ~loglevel:"always" ("mv "^pkgfile^" "^pool^"/"));

  if (not devf) && (not nodev)  then pack_rpm ~devf:true ~os ~platform chroot_name pkgspec

let pack ?(os=Platform.os ()) ?(platform=Platform.current ()) chroot_name pkgspec =
  match Platform.engine_of_platform platform with
    | Rpm_build -> pack_rpm ~os ~platform chroot_name pkgspec;
    | _ -> err "Chroot.pack" ("Unsopported platform: "^(Platform.string_of_platform platform))

(* Эта функция копирует все компоненты пакета в chroot-окружение и запускает их сборку *)
let buildpkg ?(os=Platform.os ()) ?(platform=Platform.current ()) pkgspec =
  let msg = Output.msg "Chroot.buildpkg" in
  let pkgname = pkgspec.pkgname in
  let version = pkgspec.version in
  let pack_param = Params.get "pack" in

  (* надо загрузить конфигурацию чрута *)
  let chroot = load_chroot_cfg pkgspec.chroot platform in
  let chroot_name = Filename.basename chroot.path in
  msg "always" ("Choosed chroot environment: "^pkgspec.chroot^" -> "^chroot.path);
  msg "always" ("NODEV "^(if pkgspec.nodev then "enabled" else "disabled"));
  (* раньше не было абстракции chroot-окружения, и имя было именем
     директории, в которой содержалось окружение *)
  let new_chroot_name = chroot_name^"--"^pkgname in
  (* подменяем путь чрута *)
  chroot.path <- Path.make [(Filename.dirname chroot.path); new_chroot_name];
  delete_if_exists new_chroot_name;
  copy chroot_name new_chroot_name;
  let chroot_name = new_chroot_name in (* гарантируем, что базовый chroot не будет затронут *)
  let spec = Spectype.newload ~os ~platform pkgname version in

  let topdir_opt = try (" --top-dir="^(Hashtbl.find spec.params "top-dir"))
  with _ -> "" in

  let components = List.filter (fun (comp:component) -> comp.name <> (Filename.basename pack_param))
    spec.components in
  
  msg "always" "Clone components into chroot...";
  List.iter (fun component -> ignore (clone_component chroot_name component)) components;

  msg "always" "Install build dependencies... ";
  depinstall ~os ~platform chroot_name pkgspec;

  msg "always" "Check versions of installed dependencies... (dummy)";

  begin
    try
      msg "always" "Mount supplies..."; 
      mount_all_points chroot;

      (* на случай нажатия C-c во время сборки, чтобы отмонтировались supplies *)
      let cancel_behaviour = Sys.Signal_handle
	(fun _ ->
	  msg "always" "Build has been cancelled!";
	  msg "always" "Unmount supplies..."; umount_all_points chroot;
	  exit 13) in
      Sys.set_signal Sys.sigterm cancel_behaviour;
      Sys.set_signal Sys.sigint cancel_behaviour;
      (* всё, будет отмонтироваться*)

      msg "always" "Building components in chroot...";
      List.iter (fun (component : Component.component) ->
	let name = component.name in
	let rules = Component.string_of_rules component.rules in
	ignore (Cmd.root_command ~loglevel:"low"
	  ("/bin/bf2 build-component "^chroot_name^" "^name^" "^rules
	  ^topdir_opt)))
	components;
    with _ as exn ->
      msg "always" "Build has been failed!";
      msg "always" "Unmount supplies..."; umount_all_points chroot;
      raise exn
  end;
  msg "always" "Unmount supplies..."; umount_all_points chroot;

  msg "always" ("Pack package '"^pkgname^"'"); 
  pack ~os ~platform chroot_name pkgspec;

  msg "always" ("Build of '"^pkgname^"' is complete!")

(* Эта функция строит дерево сборочных зависимостей ветви и
 * последовательно собирает листья. Разумеется, она запускает
 * несколько подпроцессов, которые выполняют основную работу и
 * сигнализируют ей о результатах.
 *)

type job_desc =
    {
      out: Buffer.t;
      err: Buffer.t;
      job: Shell_sys.job;
    }

type instance = Shell_sys.job_instance

type build_state = 
  | Build_success
  | Build_failure
  | Build_run
  | Build_wait
  | Build_not_needed
  | Build_unknown

let string_of_build_state = function
  | Build_success -> "success"
  | Build_failure -> "failure"
  | Build_run -> "running"
  | Build_wait -> "waiting"
  | Build_not_needed -> "not needed"
  | Build_unknown -> "unknown"
      
let build_subtree ?(threads=1) ?(os=Platform.os ()) ?(platform=Platform.current ()) pkgspec =
  let msg = Output.msg "build_subtree" in
  let err = Output.err "build_subtree" in
  (*let warn = Output.warn "build_subtree" in*)

  msg "always" ("THREADS: "^(string_of_int threads));

  let pack_param = Params.get "pack" in
  msg "always" ("Pull PACK repository: "^pack_param);
  System.with_dir pack_param (fun () -> Git.git_pull_new ());
  
  let top_pkg = pkgspec.pkgname in
  let version = pkgspec.version in

  (* граф сборочных зависимостей ветви + интерфейсы к нему *)
  let dg = Depgraph2.subtree_buildgraph ~local_only:true top_pkg version in
  let unreg_pkg pkg = Depgraph2.unreg_pkg dg pkg in

  let pkgs = Depgraph2.pkgs dg in
  let specs = Depgraph2.copy_spectable dg in
  let jobs = ((Hashtbl.create 0) : (pkg_name,job_desc) Hashtbl.t) in
  let instances = ((Hashtbl.create 0) : (pkg_name,instance) Hashtbl.t) in
  let status = ((Hashtbl.create 0) : (pkg_name, build_state) Hashtbl.t) in

  let pkg_spec pkg = Hashtbl.find specs pkg in
  let pkg_jdesc pkg = Hashtbl.find jobs pkg in

  (* функции для работы со статусом пакета *)
  let get_status pkg = Hashtbl.find status pkg in
  let build_ok pkg = (get_status pkg) = Build_success in
  let build_err pkg = (get_status pkg) = Build_failure in
  let build_wait pkg = (get_status pkg) = Build_wait in
  let build_unknown pkg = (get_status pkg) = Build_unknown in
  let set_status pkg st = Hashtbl.add status pkg st in
  let set_ok pkg = set_status pkg Build_success in
  let set_err pkg = set_status pkg Build_failure in
  let set_wait pkg = set_status pkg Build_wait in
  let set_run pkg = set_status pkg Build_run in
  let set_not_needed pkg = set_status pkg Build_not_needed in
  let set_unknown pkg = set_status pkg Build_unknown in

  (* функции для работы с экземпляром сборочного процесса *)
  let pkg_inst pkg = Hashtbl.find instances pkg in
  let reg_inst pkg inst = Hashtbl.add instances pkg inst in
  let unreg_inst pkg = Hashtbl.remove instances pkg in

  (* создание команды для запуска сборочного процесса *)
  let new_job pkg =
    let spec = pkg_spec pkg in
    let ver = spec.version in
    let platform = string_of_platform platform in
    let params = spec.params in
    let topdir = try ["--top-dir"; (Hashtbl.find params "top-dir")] with Not_found -> [] in
    let devdir = try ["--dev-dir"; (Hashtbl.find params "dev-dir")] with Not_found -> [] in
    let args = ["buildpkg"; pkg; ver; platform] @ topdir @ devdir in
    let command = Shell.cmd "bf2" args in
    let outbuf = Buffer.create 0 in
    let errbuf = Buffer.create 0 in
    (* судя по всему она сразу открывает дескрипторы, что не очень хорошо *)
    let (job,_) = Shell.setup_job ~stdout:(Shell.to_buffer outbuf)
      ~stderr:(Shell.to_buffer errbuf)
      [command] in
    let job = {out = outbuf; err = errbuf; job = job} in
    Hashtbl.add jobs pkg job in
  
  (* начать сборку *)
  let start_build pkg =
    msg "always" ("Start building of '"^pkg^"'");
    let jdesc = pkg_jdesc pkg in
    let job = jdesc.job in
    let inst = Shell_sys.run_job job in
    set_run pkg; reg_inst pkg inst in

  (* Хрень собачья: Shell_sys не меняет статус, пока не сделаешь
   * finish_job, который, сука, блокирует выполнению текущего треда.
   * Нужна консультация людей, которые писали Ocamlnet.
   * Ну, по крайней мере я хотя бы могу одновременно собирать листья. *)
  let wait_build pkg =
    msg "always" ("Waiting build process of '"^pkg^"'");
    Shell_sys.finish_job (pkg_inst pkg) in

  (* завершить сборку *)
  let rec stop_build pkg =
    let inst = pkg_inst pkg in
    let status = Shell_sys.job_status inst in
    (match status with
      | Shell_sys.Job_ok ->
	  msg "always" ("Build success: "^pkg);
	  unreg_inst pkg; unreg_pkg pkg; set_ok pkg
      | Shell_sys.Job_error ->
	  msg "always" ("Build failure: "^pkg);
	  unreg_inst pkg; unreg_pkg pkg; set_err pkg
      | _ -> err (""));
    (* если завершена сборка пакета, то завершена и сборка его -dev версии *)
    let devpkg = pkg^"-dev" in
    if List.mem devpkg pkgs then
      (match status with
	| Shell_sys.Job_ok ->
	    msg "always" ("Build success: "^devpkg);
	    unreg_pkg devpkg; set_ok devpkg
	| Shell_sys.Job_error ->
	    msg "always" ("Build failure: "^devpkg);
	    unreg_pkg devpkg; set_err devpkg
	| _ -> err ("")) in

  (* проверить, завершена ли сборка всех пакетов *)
  let all_builds_finished () =
    List.fold_left (fun finp pkg ->
      finp && (match (get_status pkg) with
	| Build_success | Build_failure | Build_not_needed-> true
	| _ -> false))
      true pkgs in

  (* список пакетов, которые можно стартовать *)
  let ready_to_start_pkgs () =
    List.filter build_wait (Depgraph2.leaves dg) in

  (* список пакетов с неизвестным статусом *)
  let unknown_status_pkgs () =
    List.filter build_unknown pkgs in
  
  (* список собранных пакетов *)
  let built_pkgs () =
    List.filter (fun pkg -> (build_ok pkg) (*|| (build_err pkg) || (build_not_needed pkg)*))
      pkgs in

  (* список пакетов, сборка которых завершена, для которых надо вызвать stop_build *)
  let finished_instances () =
    Hashtbl.fold (fun pkg inst acc ->
      match Shell_sys.job_status inst with
	| Shell_sys.Job_running -> acc
	| Shell_sys.Job_ok | Shell_sys.Job_error -> pkg :: acc
	| _ -> acc)
      instances [] in

  (* вывод на stdout содержимого буферов соответсвующих сборочных команд *)
  let print_inst_outputs pkg =
    let jd = pkg_jdesc pkg in
    msg "always" (Output.prefix_textblock "OUT: " (Buffer.contents jd.out));
    msg "always" (Output.prefix_textblock "ERR: " (Buffer.contents jd.err));
    List.iter Buffer.clear [jd.out; jd.err] in

  (* это дебаг для вывода инфы о состоянии пакетов *)
  let print_state () =
    print_endline "    --- Build status ---";
    List.iter (fun pkg -> print_endline ("    "^pkg^": "^(string_of_build_state (get_status pkg))))
      pkgs;
    print_endline "    --------------------" in
  
  (* -------------------- НАЧАЛО ИМПЕРАТИВА ТУТ -------------------- *)

  (* обновляем репозитории компонентов пакетов, чтобы разобарться, какие из них надо пересобирать*)
  let must_pull_repos = match Params.get "omit-pull-repos" with
    | "true" -> false
    | _ -> true in

  if must_pull_repos then
    begin
      msg "always" "Pull all the components...";
      List.iter
	(fun pkg ->
	  let spec = pkg_spec pkg in
	  let comps =
	    List.filter
	      (fun (comp:component) -> comp.name <> (Filename.basename pack_param))
	      spec.components in
	  List.iter (fun (comp:component) ->
	    if not (System.is_directory comp.name) then
	      Component.clone comp;
	    msg "low" ("pull repository of component "^comp.name);
	    System.with_dir comp.name
	      (fun () ->
		Git.git_pull_new ()))
	    comps)
	pkgs
    end
  else
    msg "always" "Pull repos is omitted because of configuration options";
  
  (* заполняем таблицу jobs командами для сборки *)
  List.iter new_job pkgs;
  (* выставляем всем пакетам статус wait *)
  List.iter set_wait pkgs;
  (* определяем список пакетов, в которых есть изменения и удаляем из графа остальные *)
  List.iter (fun pkg ->
    let spec = pkg_spec pkg in
    let comps = Package.components_need_to_rebuild spec in
    match comps with
      | [] -> (* устанавливаем статус unknown пакетам, в которых ничего не изменилось, ибо есть вероятность, что их надо перепаковать из-за строгих зависимостей от других пакетов *)
	  (*unreg_pkg pkg;*)
	  set_unknown pkg
      | _ -> ())
    pkgs;
  (* пробегаем по пакетам с неопределённым статусом и определяем его *)
  let rec determine_unknown_status_pkgs () =
    match unknown_status_pkgs () with
      | [] -> ()
      | unk_pkgs ->
	  List.iter (fun pkg ->
	    let deps = Depgraph2.pkg_deps dg pkg in
	    let (dep_from_unknown_found:bool ref) = ref false in
	    let (last_dep_found:bool ref) = ref false in
	    List.iter (fun dep ->
	      let (pkg, dep_pkg) = dep in
	      match Depgraph2.depinfo dg dep with
		| Some dep ->
		    (match dep with
		      | (Pkg_last,_) ->
			  if build_unknown dep_pkg then
			    dep_from_unknown_found := true
			  else if build_wait dep_pkg then
			    last_dep_found := true
		      | _ -> ())
		| None -> ())
	      deps;
	    if !last_dep_found then
	      set_wait pkg (* если найдёна last-зависимость, то нужна перепаковка *)
	    else if !dep_from_unknown_found then
	      () (* если нет last-dep, но есть unknown-dep, в следующую итерацию разберёмся*)
	    else
	      (* в противном случае убрать из сборочного графа эту нечисть *)
	      begin unreg_pkg pkg; set_not_needed pkg; end
	  )
	    unk_pkgs;
	  determine_unknown_status_pkgs ()
  in determine_unknown_status_pkgs ();

  (* Делаем инкремент ревизии для всех пакетов, подлежащих пересборке *)
  (* А впрочем, пусть пока так побудет *)
  
  msg "always" "Start build process";

  let i = ref 1 in
  (* сборка *)
  while not (all_builds_finished ()) do
    print_state ();
    let fin_pkgs = finished_instances () in
    List.iter stop_build fin_pkgs;
    List.iter print_inst_outputs fin_pkgs;
    let red_pkgs = ready_to_start_pkgs () in
    (* не забываем, что -dev пакеты собираются в рамках процесса сборки обычных *)
    let red_pkgs = List.remove_duplicates (List.map Pkg.chop_devel_suffix red_pkgs) in
    let red_pkgs =
      if (List.length red_pkgs) > threads then
	List.get threads red_pkgs
      else red_pkgs in
    List.iter start_build red_pkgs;
    List.iter wait_build red_pkgs; (* я жду, пока все листья соберутся, это отстой *)
    msg "always" ((string_of_int !i)^": Wave competed!");
    (*Unix.sleep 10;*)
    i := succ !i;
  done;

  (* пост-сборочные процессы *)
  let bpkgs = built_pkgs () in
  let bpkgs_wo_dev = List.filter (fun pkgname -> not (Pkg.is_devel pkgname)) bpkgs in
  
  (* расстановка тэгов и инкремент release в pack-е *)
  List.iter (fun pkg ->
    let spec = pkg_spec pkg in
    let version = spec.version in
    let specdir = Specdir.specdir_by_version pkg version in
    let revision = string_of_int (succ spec.revision) in
    let tag = Ptag.by_spec spec in
    System.with_dir specdir
      (fun () ->
	System.append_write ~file:"release" (version^" "^revision^"\n");
	Git.git_add "release";
	ignore (Git.git_make_tag tag)); (* пока не обрабатываем статус задания тэга *)
    (* в каждый компонент закидываем тэг *)
    let components = List.filter (fun (comp:component) ->
      comp.name <> (Filename.basename pack_param))
      spec.components in
    let compdir = Params.get "components-dir" in
    List.iter (fun (comp:component) ->
      System.with_dir (Path.make [compdir;comp.name])
      (fun () ->
	msg "low" ("Creating new tag for component "^comp.name);
	ignore (Git.git_make_tag tag);
	Git.git_push ~tags:true ""))
      components)
    bpkgs_wo_dev;
  System.with_dir pack_param
    (fun () ->
      Git.git_commit ~empty:true ("[BF2] Build finished: "^top_pkg^"/"^version);
      Git.git_pull "";
      Git.git_push ~tags:false "";
      Git.git_push ~tags:true "";
    );

  
  (* вывод сообщения о сборке *)
  msg "always" "--------------------------------------------------";
  msg "always" "Built packages:";
  List.iter (fun pkg ->
    if build_ok pkg then
      (let spec = pkg_spec pkg in
      let version = spec.version in
      let revision = string_of_int ((*succ*) spec.revision) in
      msg "always" ("  "^pkg^"-"^version^"-"^revision)))
    bpkgs;
  msg "always" "--------------------------------------------------";
  msg "always" "Failed packages:";
  List.iter (fun pkg ->
    if build_err pkg then
      (let spec = pkg_spec pkg in
      let version = spec.version in
      let revision = string_of_int ((*succ*) spec.revision) in
      msg "always" ("  "^pkg^"-"^version^"-"^revision)))
    bpkgs;
  ignore bpkgs; (* потом будем возвращать список собранных пакетов *)
  ()

    

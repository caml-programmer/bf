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
    | Sstring x -> x
    | _ as x -> Scheme.print x; err errmsg in
  System.with_dir (Params.get_param "chroot-spec")
    (fun () ->
     if Sys.file_exists chroot_name
     then let fullcfg = Scm.read_record (Scm.read_file chroot_name) in
	  let cfg = Scm.read_record (List.assoc platform fullcfg) in
	  let path = match List.assoc "path" cfg with
	    | Sstring p -> p
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
	  { name = chroot_name;
	    path = (Path.expand_globs path);
	    mount = mpoints; }
     else err ("Can't find chroot spec for: "^chroot_name))

exception No_chroot_command

let chroot_cmd_exists =
  let chroot = Params.get "chroot-path" in
  Sys.file_exists chroot
	    
let centos6_base_repo_filename = "Centos6-Base.repo"
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
  Params.get_param "chroots-dir"
  
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
  | Cent6 ->
     msg "always" ("Creating chroot environment for cent6 in "^chroot_dir);
     let centos_mirror = Params.get_param "centos6-mirror" in
     let centos_release_pkg_url = Repo.centos_release_pkg_url centos_mirror in
     let centos_release_pkg = Repo.pkg_by_url centos_release_pkg_url in
     let centos_release_pkg_path = Path.make [(chroots_dir ());centos_release_pkg] in

     (* разворачиваем базовое окружение *)
     ignore (root_command ("wget "^centos_release_pkg_url^" -O "^centos_release_pkg_path));
     ignore (root_command ("rpm --root "^chroot_dir^" --initdb"));
     ignore (root_command ("rpm --root "^chroot_dir^" --nodeps -ivh "^centos_release_pkg_path));

     (* заменяем репы на нужные *)
     ignore (root_command ("rm "^(Path.make [chroot_dir; "/etc/yum.repos.d/*.repo"])));
     ignore (root_command ("cp /opt/dozor/bf/data/centos6-repos/*.repo "^
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

  let bf_files = Hashtbl.create 32 in
  let reg file ftype =
    let edit_param = match Hashtbl.mem bf_files file with
      | true -> Hashtbl.replace
      | false -> Hashtbl.add in
    edit_param bf_files file ftype in
  let filecount () = Hashtbl.length bf_files in

  let components = List.filter (fun c -> c.pkg = None && (not c.nopack)) pkgspec.components in

  
  let pkgpack_dir = compose_pkgpack_dir pkgname in

  let file_rpmbuild_files = Path.make [pkgpack_dir; "rpmbuild.files"] in
  let file_rpmbuild_findreq = Path.make [pkgpack_dir; "rpmbuild.findreq"] in
  let file_rpmbuild_spec = Path.make [pkgpack_dir; "rpmbuild.spec"] in
  let buildroot = "buildroot" in
  
  (* В текущей реализации поле custom_pkg_files никак не обрабатывается
   * Какова была его изначальная задумка, мне не понятно.
   * Возможно, какие-то файлы пакета, которые заполняются при помощи хуков. *)
  let custom_pkg_files = Pkgbuild.call_before_build
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
     let depends = if devf then
		     (* dev-пакет должен зависеть от bin-пакета *)
		     (pkgspec.pkgname, Some(Pkg_eq, ver_release), None) :: pkgspec.devdeps
		   else pkgspec.depends in
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
  let find_value = function
    | "topdir" -> if nodev then "/opt" else
		    if devf then "/opt/dev"
		    else Params.get_param "top-dir"
    | "prefix" -> if nodev then "/opt" else
		    if devf then "/opt/dev"
		    else Params.get_param "top-dir"
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
     
     (match pkgspec.pre_install with
      | None -> ()
      | Some pre ->
	 print_endline "%pre";
	 print_endline "if [ \"$1\" = \"1\" ] ; then # first install";
	 print_endline "echo -n";
	 print_endline pre;
	 print_endline "fi";
	 (match pkgspec.pre_update with
	  | None -> ()
	  | Some preup ->
	     print_endline "if [ \"$1\" = \"2\" ] ; then # update";
	     print_endline "echo -n";
	     print_endline preup;
	     print_endline "fi"));
     (match pkgspec.post_install with
      | None -> ()
      | Some post ->
	 print_endline "%post";
	 print_endline post);
     (match pkgspec.pre_uninstall with
      | None -> ()
      | Some preun ->
	 print_endline "%preun";
	 print_endline "if [ \"$1\" = \"0\" ] ; then # all versions deleted";
	 print_endline "echo -n";
	 print_endline preun;
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
  
  msg "always" "Clone components into chroot...";
  List.iter (fun component -> ignore (clone_component chroot_name component))
	    spec.components;

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
		spec.components;
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

type build_state = Build_success | Build_failure
      
let build_subtree ?(os=Platform.os ()) ?(platform=Platform.current ()) pkgspec =
  let msg = Output.msg "build_subtree" in
  let err = Output.err "build_subtree" in
  let warn = Output.warn "build_subtree" in

  let top_pkg = pkgspec.pkgname in
  let version = pkgspec.version in

  (* граф сборочных зависимостей ветви + интерфейсы к нему *)
  let dg = Depgraph2.subtree_buildgraph top_pkg version in
  let unreg_pkg pkg = Depgraph2.unreg_pkg dg pkg in

  let specs = Depgraph2.copy_spectable dg in
  let jobs = ((Hashtbl.create 0) : (pkg_name,job_desc) Hashtbl.t) in
  let instances = ((Hashtbl.create 0) : (pkg_name,instance) Hashtbl.t) in
  let status = ((Hashtbl.create 0) : (pkg_name, build_state) Hashtbl.t) in

  let pkg_spec pkg = Hashtbl.find specs pkg in
  let pkg_jdesc pkg = Hashtbl.find jobs pkg in

  let get_status pkg = Hashtbl.find status pkg in
  let build_ok pkg = (get_status pkg) = Build_success in
  let build_err pkg = (get_status pkg) = Build_failure in
  let set_status pkg st = Hashtbl.add status pkg st in
  let set_ok pkg = set_status pkg Build_success in
  let set_err pkg = set_status pkg Build_failure in
  
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

  let start_job pkg =
    let jdesc = pkg_jdesc pkg in
    let job = jdesc.job in
    let inst = Shell_sys.call_job ~forward_signals:true job in
    Hashtbl.add instances pkg inst in

  let stop_job pkg =
    let inst = Hashtbl.find instances pkg in
    let status = Shell_sys.job_status inst in
    match status with
    | Shell_sys.Job_ok ->
       msg "always" ("Build success: "^pkg);
       Hashtbl.remove instances pkg;
       unreg_pkg pkg;
       set_ok pkg
    | Shell_sys.Job_error ->
       msg "always" ("Build failure: "^pkg);
       Hashtbl.remove instances pkg;
       unreg_pkg pkg;
       set_err pkg
    | _ -> err ("") in
  
  
  ()
		
    
    
  
(*
type process_fds = (Unix.file_descr * Unix.file_descr * Unix.file_descr)
let fdin ((fd, _, _) : process_fds) = fd
let fdout ((_, fd, _) : process_fds) = fd
let fderr ((_, _, fd) : process_fds) = fd
		     
type process_id = int
type build_status = Built of pkg_rev | (* собрано с ревизией *)
		    Building of process_id | (* в процессе сборки *)
		    Failed of Unix.process_status | (* сборка провалилась *)
		    Waiting (* ожидает сборки *)

exception Process_finished of process_id
exception Found
				
let build_subtree ?(os=Platform.os ()) ?(platform=Platform.current ()) pkgspec =
  let msg = Output.msg "build_subtree" in
  let err = Output.err "build_subtree" in
  let warn = Output.warn "build_subtree" in

  let top_pkg = pkgspec.pkgname in
  let version = pkgspec.version in

  (* таблица буферов считывания из файловых дескрипторов *)
  let fd_table = ((Hashtbl.create 0) : (Unix.file_descr, Buffer.t) Hashtbl.t) in
  let reg_fd fd =
    Hashtbl.add fd_table fd (Buffer.create 0) in
  let unreg_fd fd =
    let buf = Hashtbl.find fd_table fd in
    Hashtbl.remove fd_table fd; buf in
  let fd_buf fd = Hashtbl.find fd_table fd in
  let append_buf fd chunk len =
    let buf = fd_buf fd in
    if len <> 0 then Buffer.add_string buf chunk in

  (* таблица процессов и связанных с ними файловых дескрипторов *)
  let process_table = ((Hashtbl.create 0) : (process_id, process_fds) Hashtbl.t) in
  let get_fds pid = Hashtbl.find process_table pid in
  let reg_process (pid,fds) =
    Hashtbl.add process_table pid fds;
    reg_fd (fdout fds);
    reg_fd (fderr fds) in
  let unreg_process pid =
    let (fdin,fdout,fderr) = get_fds pid in
    Hashtbl.remove process_table pid;
    let outputs = unreg_fd fdout in
    let errors = unreg_fd fderr in
    let status = Cmd.async_status pid in
    Unix.close fdin; Unix.close fdout; Unix.close fderr;
    (status, outputs, errors) in

  let fd_pid = Hashtbl.fold
		 (fun pid fds
  
  (* граф сборочных зависимостей ветви + интерфейсы к нему *)
  let dg = Depgraph2.subtree_buildgraph top_pkg version in
  let pkg_spec pkg = Depgraph2.pkg_spec dg pkg in
  let unreg_pkg pkg = Depgraph2.unreg_pkg dg pkg in

  (* таблица статусов обработки пакетов дерева сборочных зависимостей *)
  let build_table = ((Hashtbl.create 0) : (spec, build_status) Hashtbl.t) in
  let set_status pkg status =
    let spec = pkg_spec pkg in
    Hashtbl.add build_table spec status in
  let get_status pkg =
    let spec = pkg_spec pkg in
    Hashtbl.find build_table spec in
  
  (* дополнительные функции *)
  (* возвращает истину, если сборка всех пакетов завершена *)
  let build_finished () =
    Hashtbl.fold (fun pkgspec status finp ->
		  let pkg_finp = match status with
		    | Built _ | Failed _ -> true
		    | Building _ | Waiting -> false in
		  finp && pkg_finp)
		 build_table true in
  (* возвращает истину, если сборка ещё идёт *)
  let build_in_process () = not (build_finished ()) in
  (* возвращает список пакетов, которые можно начать собирать *)
  let ready_to_start_pkgs () = Depgraph2.leaves dg in
  (* возвращает список пакетов, находящихся в процессе сборки *)
  let pkgs_that_are_building () =
    Hashtbl.fold (fun spec status acc ->
		  match status with
		  | Building _ -> spec.pkgname :: acc
		  | _ -> acc)
		 build_table [] in

  
  (* ищет, какой пакет собирал pid *)
  let pid_pkg pid =
    let pkg =
      Hashtbl.fold
	(fun spec status result ->
	 if result <> "" then result
	 else match status with
	      | Building pid_to_check ->
		 if pid = pid_to_check then spec.pkgname else ""
	      | _ -> "")
	build_table "" in
    if pkg <> "" then pkg else raise Not_found in
  
  (* запускает новую сборку, требует от sudo сохранения HOME (херня, да) *)
  let start_build pkg =
    if (get_status pkg) <> Waiting then
      err ("package '"^pkg^"' does not need building");
    try let version = (pkg_spec pkg).version in
	let platform_str = string_of_platform platform in
	let (pid,fds) = Cmd.async_command ("sudo bf2 buildpkg "^pkg^" "^version^" "^platform_str) in
	reg_process (pid,fds);
	set_status pkg (Building pid)
    with Unix.Unix_error (err, func, arg) ->
      warn (Output.string_of_string_list
	      [("Unix error: "^(Unix.error_message err));
	       ("Function: "^func); ("Argument: "^arg)]) in

  (* завершает сборку *)
  let finish_build pkg =
    let rev = succ (pkg_spec pkg).revision in
    match get_status pkg with
    | Building pid ->
       let (pstatus, outputs, errors) = unreg_process pid in
       unreg_pkg pkg; (* удаление из графа *)
       set_status pkg (Built rev);
       (pstatus, outputs, errors)
    | _ -> err ("package '"^pkg^"' is not building now!") in
       
  (* устанавливаем всем пакетам статус Waiting *)
  List.iter (fun pkg -> set_status pkg Waiting) (Depgraph2.pkgs dg);

  (* вычитывает содержимое пайпов из соответствующих файловых
  дескрипторов в ассоциированные с ними буферы. кидает
  Process_finished если процесс завершился *)
  let select_timeout = 0.1 in
  let chunk_size = 4 * int_of_float(2.**8.) in
  let chunk = Bytes.create 0 in

  let read_fd_to_end fd =
    let n = ref 0 in
    n := Unix.read fd chunk 0 chunk_size;
    while !n <> 0 do
      append_buf fd chunk;
      n := Unix.read fd chunk 0 chunk_size
    done in

  let ipc () =
    (*pkgs_that_are_building ()*)

    List.iter
      (fun pkg ->
       let status = get_status pkg in
       match status with
       | Building pid ->
	  begin
	    let (infd, outfd, errfd) = get_fds pid in
	    let ready_fds = Unix.select [outfd; errfd] [] [] select_timeout in
	    let output_finished_p = ref false in
	    let error_finishd_p = ref false in
	    (* считываем кусок для outfd *)
	    let n = Unix.read outfd chunk 0 chunk_size in
	    if n = 0 then output_finished_p := true
	    else append_buf outfd chunk n;
	    (* считываем кусок для errfd *)
	    let n = Unix.read errfd chunk 0 chunk_size in
	    if n = 0 then error_finishd_p := true
	    else append_buf errfd chunk n;
	    (* если один дескриптор дочитан, дочитываем и второй *)
	    match !output_finished_p, !error_finishd_p with
	    | false,false  | true, true -> ()
	    | true,false -> read_fd_to_end errfd; raise (Process_finished pid)
	    | false, true -> read_fd_to_end outfd; raise (Process_finished pid)
	  end
       | _ -> ())
      (pkgs_that_are_building ()) in

    
  
  

  let rec build_loop () =
    if build_in_process () then
      try
	List.iter start_build (ready_to_start_pkgs ());
      with
      | Process_finished pid ->
	 let pkg = pid_pkg pid in
       *)

      

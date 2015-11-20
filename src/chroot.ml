open Output
open Platform
open Printf
open Component
open Spectype

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
  Params.get_param "projects-dir"
		 
let compose_projects_path chroot_name =
  Path.make [(compose_chroot_path chroot_name); (projects_path ())]

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

     let oldstate = Scanner.create_top_state install_dir in
     let oldstate_dev = Scanner.create_top_state dev_dir in
     if dest_dir <> "" then
       Params.update_param "top-dir" install_dir; (* Deprecated: use install-dir *)
     Rules.install_rules rules;
     Params.update_param "top-dir" top_dir;
     Params.update_param "dev-dir" dev_dir_orig;
     let newstate = Scanner.create_top_state install_dir in
     let newstate_dev = Scanner.create_top_state dev_dir in
     Scanner.generate_changes rules top_dir oldstate newstate;
     Scanner.generate_changes ~devlist:true rules dev_dir oldstate_dev newstate_dev;
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
      let full_pkgs = List.map (fun pkg -> Path.make [pool_dir; pkg]) local_pkgs in
      let full_pkgs_str = Output.string_of_string_list ~separator:" " full_pkgs in
      ignore (Cmd.root_command ~loglevel:"always" ("cp "^full_pkgs_str^" "^chroot_dir^"/"));
      let pkgs = List.map (fun pkg -> Path.make ["/"; pkg]) local_pkgs in
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

  (* В текущей реализации поле custom_pkg_files никак не обрабатывается
   * Какова была его изначальная задумка, мне не понятно.
   * Возможно, какие-то файлы пакета, которые заполняются при помощи хуков. *)
  let custom_pkg_files = Pkgbuild.call_before_build
			   ~snapshot:false ~pkgname:pkgname
			   ~version ~revision:(string_of_int revision)
			   ~platform None in
  
  (* генерируем список файлов *)
  let file_rpmbuild_files = "rpmbuild.files" in
  msg "always" ("Generating file "^file_rpmbuild_files);
  List.iter
    (fun component ->
     System.with_dir (Path.make [projects_path; component.name])
       (fun () ->
	let reg_file bf_list_file =
	  let bf_list = System.list_of_file bf_list_file in
	  List.iter (fun str ->
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
  let file_rpmbuild_files = Path.make [(Sys.getcwd ()); file_rpmbuild_files] in

  (* генерируем findreq -- скрипт, определяющий в rpmbuild список зависимостей *)
  let file_rpmbuild_findreq = "rpmbuild.findreq" in
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
  let file_rpmbuild_findreq = Path.make [(Sys.getcwd ()); file_rpmbuild_findreq] in
	  
  (* генерируем spec-файл для rpmbuild *)
  let file_rpmbuild_spec = "rpmbuild.spec" in
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
    | "buildroot" -> "buildroot"
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
  let file_rpmbuild_spec = Path.make [(Sys.getcwd ()); file_rpmbuild_spec] in

  (* Копируем в $chroot/buildroot файлы *)
  ignore (command ~loglevel:"always" chroot_name
	    "sh -c 'test -d /buildroot && rm -rf /buildroot || true'");
  ignore (Cmd.root_command ~loglevel:"always"
	    ("/bin/bf copy-to-buildroot "^chroot_name^" "^file_rpmbuild_files^" /buildroot"));
  
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
let buildpkg ?(os=Platform.os ()) ?(platform=Platform.current ()) chroot_name pkgspec =
  let msg = Output.msg "Chroot.buildpkg" in
  let pkgname = pkgspec.pkgname in
  let version = pkgspec.version in
  let new_chroot_name = chroot_name^"--"^pkgname in
  delete_if_exists new_chroot_name;
  copy chroot_name new_chroot_name;
  let chroot_name = new_chroot_name in (* гарантируем, что базовый chroot не будет затронут *)
  let spec = Spectype.newload ~os ~platform pkgname version in

  msg "always" "Clone components into chroot...";
  List.iter (fun component -> ignore (clone_component chroot_name component))
	    spec.components;

  msg "always" "Install build dependencies... ";
  depinstall ~os ~platform chroot_name pkgspec;

  msg "always" "Check versions of installed dependencies...";
  
  msg "always" "Building components in chroot...";
  let chroot_proc_path = (compose_chroot_path chroot_name)^"/proc" in
  ignore (Cmd.root_command ~loglevel:"low" ("mount -t proc none "^chroot_proc_path));
  (* на случай нажатия C-c во время сборки, чтобы отмонтировался /proc *)
  let cancel_behaviour = Sys.Signal_handle (fun _ ->
    msg "always" "Build has been cancelled!";
    ignore (Cmd.root_command ~loglevel:"low" ("umount "^chroot_proc_path));
    exit 13) in
  Sys.set_signal Sys.sigterm cancel_behaviour;
  Sys.set_signal Sys.sigint cancel_behaviour;
  (* всё, будет отмонтироваться*)
  begin
    try
      List.iter (fun component ->
		 let name = component.name in
		 let rules = Component.string_of_rules component.rules in
		 ignore (Cmd.root_command ~loglevel:"low"
			  ("/bin/bf build-component "^chroot_name^" "^name^" "^rules)))
		spec.components;
    with _ as exn ->
      msg "always" "Build has been failed!";
      ignore (Cmd.root_command ~loglevel:"low" ("umount "^chroot_proc_path));
      raise exn
  end;
  ignore (Cmd.root_command ~loglevel:"low" ("umount "^chroot_proc_path));
      

  msg "always" ("Pack package '"^pkgname^"'"); 
  pack ~os ~platform chroot_name pkgspec;
  
  msg "always" ("Build of '"^pkgname^"' is complete!")

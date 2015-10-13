open Output
open Platform
open Printf
open Component
open Spectype
       
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

(* если chroot_name начинается на /, то он и возвращается, считаясь
   абсолютным путём к chroot-окружению. В противном случае
   возвращается $home/$chroots-dir/$chroot_name *)
let compose_chroot_path chroot_name =
  if Path.is_absolute chroot_name
  then chroot_name
  else let home_dir = compose_home_path () in
       let chroots_dir = Path.make [home_dir; (Params.get_param "chroots-dir")] in
       Path.make [chroots_dir; chroot_name]

let projects_path () =
  Params.get_param "projects-dir"
		 
let compose_projects_path chroot_name =
  Path.make [(compose_chroot_path chroot_name); (projects_path ())]
	      
(* Эта функция создаёт chroot-окружение с именем chroot_name в
   директории, описываемой параметром chroots-dir. В этом окружении
   разворачивается базовая система platform *)
let make chroot_name platform =
  let err msg = err "Chroot.make" msg in
  let unsupported_platform platform = err ("Unsupported platform " ^ (string_of_platform platform)) in
  let home_dir = compose_home_path () in
  let chroots_dir = Path.make [home_dir; (Params.get_param "chroots-dir")] in
  System.mkdir_if_not_exists chroots_dir;
  let chroot_dir = Path.make [chroots_dir; chroot_name] in
  (if (Sys.file_exists chroot_dir) && (Sys.is_directory chroot_dir)
   then System.remove_directory chroot_dir);
  ignore (Unix.mkdir chroot_dir 0o755);
  match platform with
  | Cent6
  | Cent7 ->
     let centos_mirror = Params.get_param "centos-mirror" in
     let repo = centos_mirror
		^ (match platform with
		   | Cent6 -> "6"
		   | Cent7 -> "7"
		   | p -> unsupported_platform p )
		^ "/os/x86_64/" in
     let centos_packages_dir = Params.get_param "centos-packages-dir" in
     let repo_packages = repo ^ centos_packages_dir ^ "/" in
     let (_,centos_release_pkg,_) =
       Cmd.command ("curl -s "^repo_packages^" | grep -Po 'centos-release.*?\\.rpm' | head -1") in
     let centos_release_pkg_path = Path.make [chroots_dir;centos_release_pkg] in
     ignore (Cmd.command ("wget "^repo_packages^centos_release_pkg^" -O "^centos_release_pkg_path));
     ignore (Cmd.command ("fakeroot fakechroot rpm --root "^chroot_dir^" --initdb"));
     ignore (Cmd.command ("fakeroot fakechroot rpm --root "^chroot_dir^" --nodeps -ivh "^centos_release_pkg_path));
     ignore (Cmd.command ("rm "^(Path.make [chroot_dir; "etc/yum.repos.d/*.repo"])));

     ignore (Cmd.command ("mkdir -p "^(Path.make [chroot_dir; "projects/"])));
     ignore (Cmd.command ("cp .bf-params "^(Path.make [chroot_dir; "projects/"])));

     ignore (Cmd.command ("mkdir -p "^(Path.make [chroot_dir; "/bin"])));
     ignore (Cmd.command ("cp /bin/bf "^(Path.make [chroot_dir; "/bin"])))
  | p -> unsupported_platform p

(* Эта функция копирует компонент в директорию $chroot_name/projects *)
let clone_component chroot_name component =
  Component.checkout_new component;
  let component_path = "file://"^(Path.make_absolute (Component.path component)) in
  System.with_dir (compose_projects_path chroot_name)
		  (fun () -> Component.clone_last ~from:component_path component)

(* Эта функция вызывается из корня chroot-окружения, переходит в
каталог, куда склонированы проекты, а затем: переходит там в
директорию компонента, собирает его и устанавливает в окружение.*)
let build_component component_name rules =
  let projects_relative_path = projects_path () in
  let project_path = Path.make [projects_relative_path; component_name] in
  System.with_dir project_path
    (fun () ->
     Rules.build_rules rules;
     Rules.install_rules rules)

(* Эта функция копирует все компоненты пакета в chroot-окружение и
запускает их сборку *)
let buildpkg ?(os=Platform.os ()) ?(platform=Platform.current ()) chroot_name pkgname version =
  let spec = Spectype.newload ~os ~platform pkgname version in
  let chroot_path = compose_chroot_path chroot_name in
  List.iter (fun component -> ignore (clone_component chroot_name component))
	    spec.components;
  List.iter (fun component ->
	     let name = component.name in
	     let rules = Component.string_of_rules component.rules in
	     (ignore (Cmd.command_log ("fakeroot fakechroot chroot " ^ chroot_path ^ " " ^
					 "/bin/bf build-component "^name^" "^rules))))
	    spec.components
    

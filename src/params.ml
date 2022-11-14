open Ocs_env
open Ocs_types
open String_ext
       
exception Unknown_parameter of string

let user_params = Hashtbl.create 32;;

let debug =
  try
    ignore(Sys.getenv "DEBUG");
    true
  with Not_found -> false

let read_from_file filename =
  let rex = Re.Perl.compile_pat "^([^\\s]+)\\s+(.*)\\s*$" in
  if Sys.file_exists filename
  then let ch = open_in filename in
       List.iter
	 (fun s ->
	  try
	    let res = Core.exec rex s in
	    let key = Core.Group.get res 1 in
	    let value = Core.Group.get res 2 in
	    Hashtbl.replace user_params key value;
	    Ocs_env.set_glob Scheme.env
	      (Ssymbol key) (Sstring (Bytes.of_string value))
	  with Not_found ->
	    Printf.printf "ignore: \"%s\"\n%!" s)
	 (List.filter String.not_empty (System.list_of_channel ch));
       user_params
  else Hashtbl.create 0

let read_params () =
  let default =
    let def = "/etc/bf-params" in
    if Sys.file_exists def then
      Some def
    else
      None in
  match System.up_search ~default ".bf-params" with
    | None -> Hashtbl.create 32
    | Some filename ->
       if debug then
	 Printf.printf "loading %s\n%!" filename;
       read_from_file filename

let set_param ~default s =
  let value =
    try
      Hashtbl.find user_params s
    with Not_found -> default
  in
  
  Ocs_env.set_glob Scheme.env
    (Ssymbol s) (Sstring (Bytes.of_string value));
  
  Hashtbl.replace user_params s value

let exists param =
  Hashtbl.mem user_params param
		  
let set param value =
  (*print_endline ("Set param: "^param^" -> "^value);*)
  if not (exists param) then
    raise (Unknown_parameter param);
  Ocs_env.set_glob Scheme.env (Ssymbol param) (Sstring (Bytes.of_string value));
  Hashtbl.replace user_params param value

let create param value =
  if exists param then
    failwith ("Parameter already exists: "^param);
  Ocs_env.set_glob Scheme.env (Ssymbol param) (Sstring (Bytes.of_string value));
  Hashtbl.add user_params param value

let get_param s =
  try
    Hashtbl.find user_params s
  with Not_found -> raise (Unknown_parameter s)

let get = get_param
			  
let update_param name value =
  Ocs_env.set_glob Scheme.env
    (Ssymbol name) (Sstring (Bytes.of_string value));
  Hashtbl.replace user_params name value

let set_composite_mode () =
  update_param "composite-mode" "true"

let disable_display_logs () =
  update_param "display-command-logs" "false";
  update_param "log-level" "low"

let enable_display_logs () =
  update_param "display-command-logs" "true";
  update_param "log-level" "high"

let used_composite_mode () =
  match get_param "composite-mode" with
    | "false" -> false
    | "true"  -> true
    | _       -> assert false

let reread_params () =
  Hashtbl.clear user_params;
  set_param ~default:(Sys.getcwd()) "top-dir";
  set_param ~default:(Sys.getcwd()) "dev-dir";
  set_param ~default:"" "dest-dir";
  set_param ~default:"logs" "log-dir";
  set_param ~default:"git://localhost/" "git-url";
  set_param ~default:"bf" "component";
  set_param ~default:"branch" "label-type";
  set_param ~default:"master" "label";
  set_param ~default:"." "plugins-dir";
  set_param ~default:"false" "composite-mode";
  set_param ~default:(Sys.getcwd()) "start-dir";
  set_param ~default:"low" "log-level"; (* low,high *)
  set_param ~default:"" "make-opts";

  set_param ~default:"release" "build-type"; (* debug or release *)
  set_param ~default:"stop-delay" "5";

  set_param ~default:"localhost" "smtp-server";
  set_param ~default:"25" "smtp-port";
  set_param ~default:"bf message" "smtp-subject";
  set_param ~default:"bf" "smtp-from-name";
  set_param ~default:"bf@notify" "smtp-from-mail";
  set_param ~default:"" "smtp-notify-email"; (* used by changelog action *)
  set_param ~default:"true" "autopkg";
  set_param ~default:"yum.solar.local" "pkg-storage";
  set_param ~default:"jet-vas" "pkg-branch";
  set_param ~default:"" "http-proxy";

  set_param ~default:"jet" "pkg-prefix";
  set_param ~default:"" "pkg-prefix-exclude";
  set_param ~default:"bf.session.log" "session-log";
  set_param ~default:"bf.lock" "lock-file";
  set_param ~default:"true" "use-external"; (* components in external packages *)
  set_param ~default:"false" "single-pack-fetch"; (* for upgrade action *)
  set_param ~default:"graphs" "graph-home"; (* for *graph actions *)  

  set_param ~default:"false" "clone-mode"; (* for hooks *)
  set_param ~default:"pack" "pack";
  set_param ~default:"link-mode" "hard"; (* or "soft" for external relinking *)
  set_param ~default:"true" "display-command-logs";

  set_param ~default:"" "jira-host";
  set_param ~default:"8080" "jira-port"; (* http *)
  set_param ~default:"jirabot" "jira-user";
  set_param ~default:"ahng6Ije" "jira-pass";

  set_param ~default:"http://mirror.yandex.ru/centos/6/os/x86_64/" "centos6-mirror";

  set_param ~default:"Packages" "centos-packages-dir";
  set_param ~default:"http://mirror.yandex.ru/debian/" "debian-mirror";
  set_param ~default:"Packages" "debian-packages-dir";

  (* в этой директории лежат chroot-окружения*)
  set_param ~default:"chroots" "chroots-dir";
  (* в этой директории лежат описания chroot-окружений *)
  set_param ~default:"chroot-spec" "chroot-spec";
  (* в эту директорию в chroot-окружении копируются снимки репозиториев для сборки*)
  set_param ~default:"projects" "projects-dir";
  (* в этой переменной указан полный путь до chroot *)
  set_param ~default:"/usr/sbin/chroot" "chroot-path";
  (* в этой переменной указан полный путь до sudo *)
  set_param ~default:"/usr/bin/sudo" "sudo-path";
  (* в этой переменной указан полный путь до su *)
  set_param ~default:"/bin/su" "su-path";
  (* если этот параметр установлен в true, то в случае одновременного
  наличия в системе su и sudo предпочтение отдаётся sudo *)
  set_param ~default:"true" "prefer-sudo";
  (* в эту директорию складируются собранные пакеты, и в ней же ищутся
  пакеты, удовлетворяющие сборочные зависимости *)
  set_param ~default:"pool" "pool-dir";
  (* в этой директории находятся каталоги, которые опционально (в
  зависимости от chroot-spec) монтируются в chroot-окружения *)
  set_param ~default:"~/mount-supplies" "mount-supplies-path";
  (* этот параметр полезен, пока не все specdir-ы содержат файл
  chroot, из-за отсутствия которого загрузка spec-а пакета при помощи
  newload рушится *)
  set_param ~default:"false" "omit-chroot-while-specload";
  (* в этой директории создаются диретории с именами собирающихся
  пакетов, в которых создаются файлы для пакетирования при помощи,
  например, rpmbuild *)
  set_param ~default:"pkgpack" "pkg-pack-dir";
  (* этот параметр указывает на версию bf, которая в данный момент
  используется *)
  set_param ~default:"1" "bf-version";
  (* префикс тэга (сам тэг имеет вид: [prefix][pkg]/[ver]-[rev] *)
  set_param ~default:"" "tag-prefix";
  (* директория с компонентами *)
  set_param ~default:"." "components-dir";
  (* директория, из которой надо работать (актуально для совместимости с bf) *)
  set_param ~default:"." "root-dir";
  (* для того, чтобы не делать pull репозиториев перед сборкой: полезно для отладки *)
  set_param ~default:"false" "omit-pull-repos";
  ()
  

(* Utils *)

let make_install_dir () =
  let top_dir = get_param "top-dir" in
  let dest_dir = get_param "dest-dir" in
  if dest_dir = "" then
    top_dir
  else
    Filename.concat dest_dir
      (System.strip_root top_dir)

let dest_dir () =
  let dest_dir = get_param "dest-dir" in
  if dest_dir <> "" then
    Some dest_dir
  else None

let home_made_package pkg =
  let exclude =
    try
      let ex_prefix_str = get_param "pkg-prefix-exclude" in
      let ex_prefix_list = Str.split (Str.regexp "[ ]+") ex_prefix_str in
      List.exists
	(fun ex_prefix ->
	  String.length ex_prefix <> 0 && Strings.have_prefix ex_prefix pkg)
	ex_prefix_list
    with Unknown_parameter _ -> false in
  (Strings.have_prefix (get_param "pkg-prefix") pkg) && not exclude

exception Bad_specdir of string
exception No_pkg_prefix of string

let update_for_specdir specdir =
  let update =
    try
      ignore(Sys.getenv "NOT_UPDATE_BY_SPECDIR");
      false
    with Not_found -> true
  in
  if update then
    begin
      match
	List.filter 
	  (function 
	    | ""  -> false 
	    | "." -> false 
	    | _   -> true)
	  (Strings.split '/' specdir)
      with
	| pack::pkgname::packbranch::_ ->
	    let pkg_prefix =
	      try
		let pos = String.index pkgname '-' in
		String.sub pkgname 0 (pred pos)
	      with Not_found ->
		raise (No_pkg_prefix pkgname) in
	    let plugins_dir = pack in
	    update_param "pkg-prefix" pkg_prefix;
	    update_param "pack" pack;
	    update_param "plugins-dir" plugins_dir
	| _ ->
	    raise (Bad_specdir specdir)
    end


let _ =
  reread_params ();
  read_params ()

open System
open Ocs_env
open Ocs_types

exception Unknown_parameter of string

let user_params = Hashtbl.create 32;;

let read_from_file filename =
  let rex = Re_perl.compile_pat "^([^\\s]+)\\s+(.*)\\s*$" in
  if Sys.file_exists filename
  then let ch = open_in filename in
       List.iter
	 (fun s ->
	  try
	    let res = Re.exec rex s in
	    let key = Re.get res 1 in
	    let value = Re.get res 2 in	
	    Hashtbl.replace user_params key value;
	    Ocs_env.set_glob Scheme.env
			     (Ssymbol key) (Sstring value)
	  with Not_found ->
	    Printf.printf "ignore: %s\n%!" s)
	 (list_of_channel ch);
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
	Printf.printf "loading %s\n%!" filename;
	read_from_file filename

let set_param ~default s =
  let value =
    try
      Hashtbl.find user_params s
    with Not_found -> default
  in
  
  Ocs_env.set_glob Scheme.env
    (Ssymbol s) (Sstring value);
  
  Hashtbl.replace user_params s value

let get_param s =
  try
    Hashtbl.find user_params s
  with Not_found -> raise (Unknown_parameter s)

let get = get_param
			  
let update_param name value =
  Ocs_env.set_glob Scheme.env
    (Ssymbol name) (Sstring value);
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

  set_param ~default:"http://mirror.yandex.ru/centos/" "centos-mirror";
  set_param ~default:"Packages" "centos-packages-dir";
  set_param ~default:"chroots" "chroots-dir";
  set_param ~default:"projects" "projects-dir";
  
  read_params ()

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
      List.exists (fun ex_prefix -> String.length ex_prefix <> 0 && Strings.have_prefix ex_prefix pkg) ex_prefix_list
    with Unknown_parameter _ -> false in
  (Strings.have_prefix (get_param "pkg-prefix") pkg) && not exclude

exception Bad_specdir of string
exception No_pkg_prefix of string

let update_for_specdir specdir =
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



let _ =
  reread_params ()

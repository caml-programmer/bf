open System
open Ocs_env
open Ocs_types

exception Unknown_parameter of string

let read_from_file filename =
  let params = Hashtbl.create 32 in
  let rex = Pcre.regexp "^([^\\s]+)\\s+(.*)$" in
  let ch = open_in filename in
  List.iter
    (fun s ->
      if Pcre.pmatch ~rex s then
	let a =
	  Pcre.extract ~rex s
	in Hashtbl.replace params a.(1) a.(2))
    (list_of_channel ch);
  params  

let read_params () =
  let filename =
    if Sys.file_exists ".bf-params" then
      Some ".bf-params"
    else
      if Sys.file_exists "/etc/bf-params"
      then Some "/etc/bf-params" else None
  in match filename with
    | None ->  Hashtbl.create 32
    | Some filename ->
	read_from_file filename
;;

let user_params = 
  read_params ()
;;

let set_param ~default s =
  let value =
    try
      Hashtbl.find user_params s
    with Not_found -> default
  in
  
  Ocs_env.set_glob Scheme.env
    (Ssymbol s) (Sstring value);
  
  Hashtbl.replace user_params s value
;;

let get_param s =
  try 
    Hashtbl.find user_params s
  with Not_found -> raise (Unknown_parameter s)
;;

let update_param name value =
  Ocs_env.set_glob Scheme.env
    (Ssymbol name) (Sstring value);
  Hashtbl.replace user_params name value
;;

let set_composite_mode () =
  update_param "composite-mode" "true"
;;

let used_composite_mode () =
  match get_param "composite-mode" with
    | "false" -> false
    | "true"  -> true
    | _       -> assert false
;;

set_param ~default:(Sys.getcwd()) "top-dir";;
set_param ~default:(Sys.getcwd()) "dev-dir";;
set_param ~default:"" "dest-dir";;
set_param ~default:"logs" "log-dir";;
set_param ~default:"git://localhost/" "git-url";;
set_param ~default:"bf" "component";;
set_param ~default:"branch" "label-type";;
set_param ~default:"master" "label";;
set_param ~default:"." "plugins-dir";;
set_param ~default:"false" "composite-mode";;
set_param ~default:(Sys.getcwd()) "start-dir";;
set_param ~default:"low" "log-level";; (* low,high *)
set_param ~default:"" "make-opts";;

set_param ~default:"localhost" "smtp-server";;
set_param ~default:"25" "smtp-port";;
set_param ~default:"bf message" "smtp-subject";;
set_param ~default:"bf" "smtp-from-name";;
set_param ~default:"bf@notify" "smtp-from-mail";;
set_param ~default:"bf@notify" "smtp-notify-email";; (* used by changelog action *)
set_param ~default:"false" "autopkg";;
set_param ~default:"microball.lpr.jet.msk.su" "pkg-storage";;

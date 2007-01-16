open System

let read_params () =
  let params = Hashtbl.create 32 in
  let filename =
    if Sys.file_exists ".bf_params" then
      Some ".bf-params"
    else
      if Sys.file_exists "/etc/bf-params"
      then Some "/etc/params" else None
  in match filename with
    | None -> params
    | Some filename ->
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
;;

let user_params = 
  read_params ()
;;

let set_param ~default s =
  try
    Hashtbl.find user_params s
  with Not_found -> default
;;

let top_dir = set_param ~default:(Sys.getcwd()) "top-dir";;
let log_dir = set_param ~default:(top_dir ^ "/logs") "log-dir";;
let git_url = set_param ~default:"git://localhost/" "git-url";;
let prefix  = set_param ~default:"/opt/dozor" "prefix";;
let destdir = set_param ~default:"/" "destdir";;
let component = ref "";;

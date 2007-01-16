(*
 * Work with SKVT environment.
 *)

exception Env_variable_not_found of string
exception Cannot_get_password_entry of string
exception Cannot_get_group_entry of string

let get_env s =
  try
    Sys.getenv s
  with Not_found ->
    raise (Env_variable_not_found s)
    
let get_pw_entry user =
  try
    Unix.getpwnam user
  with
      Not_found ->
	raise (Cannot_get_password_entry user)

let env_prepare () =
  let real_user = get_env "USER" in
  let dozor_user = get_env "DOZOR_USER" in
  let dozor_group = get_env "DOZOR_GROUP" in
  let dozor_home = get_env "DOZOR_HOME" in
  
  let pw_entry = get_pw_entry dozor_user in
  let uid = pw_entry.Unix.pw_uid in
  let gid = pw_entry.Unix.pw_gid in

  let set_dozor_env () =
    (try
      Unix.setgid gid;
      Unix.setuid uid;
    with
	Unix.Unix_error (error,func,_) ->
	  Printf.eprintf "%s, %s\n" (Unix.error_message error) func;
	  exit 1)
  in
  
  if dozor_user <> real_user || (not (Sys.file_exists dozor_home)) then
    set_dozor_env ()

    



(*  Модуль интеграции с Jira.
 *
 * Необходим, чтобы проставлять в исправленные
 * баги номера версий пакетов.
 *)

open Printf

IFDEF YOJSON THEN
module J = Yojson.Safe

exception Bad_jira_port of string
exception Bad_response_status of string
exception Bad_json_path of string
exception Bad_json_value of J.json

let debug =
  try
    ignore(Sys.getenv "DEBUG");
    true
  with Not_found -> false

let jira_field = "customfield_10802"
let jira_host = Params.get_param "jira-host"
let jira_port =
  let port =
    Params.get_param "jira-port" in
  try
    int_of_string port
  with _ -> raise (Bad_jira_port port)
let jira_user = Params.get_param "jira-user"
let jira_pass = Params.get_param "jira-pass"

let basic = B64.encode (sprintf "%s:%s" jira_user jira_pass)

let mkurl key =
  sprintf "http://%s:%d/rest/api/2/issue/%s" jira_host jira_port key

let mkget key =
  sprintf "GET /rest/api/2/issue/%s HTTP/1.1\r\nHost: %s\r\nAuthorization: Basic %s\r\n\r\n" key jira_host basic

let mkput key data =  
  sprintf "PUT /rest/api/2/issue/%s HTTP/1.1\r\nHost: %s\r\nContent-Type: application/json\r\nContent-Length: %d\r\nAuthorization: Basic %s\r\n\r\n%s\r\n" 
    key jira_host (String.length data) basic data

let http query =
  Logger.log_message (sprintf "[jira]: connect to %s:%d user=%s pass=%s" jira_host jira_port jira_user jira_pass);
  let io =
    Unix.open_connection
      (Unix.ADDR_INET
	((Unix.gethostbyname jira_host).Unix.h_addr_list.(0),jira_port)) in
  let log =
    if Params.get_param "log-level" = "high" || debug then
      print_string
    else (fun _ -> ())
  in
  let (code,header,body) =
    Http.call_request log io query in
  Unix.shutdown_connection (fst io);
  let code = int_of_string code in
  if code < 200 || code >= 300 then
    raise (Bad_response_status (sprintf "%d, %s" code body))
  else
    if body = "" then
      `Null
    else
      J.from_string body

let get_issue key =
  http (mkget key)

let update_issue key data =
  ignore(http (mkput key data))

let rec extract value path =
  match path with
    | [] -> value
    | key::tl ->
	match value with
	  | `Assoc l ->
	      let value =
		List.assoc key l in
	      if debug then
		printf "[jira]: extract: %s -> %s\n%!" key (J.to_string value);
	      (match value with
		| `Null -> `Null
		| _ -> extract value tl)
	  | _ ->
	      raise (Bad_json_path (String.concat "/" path))

let mkstrlist v =
  match v with
    | `Null -> []
    | `List l ->
	List.map 
	  (function 
	    | `String s -> s
	    | _ -> raise (Bad_json_value v)) l
    | _ -> raise (Bad_json_value v)

let fix_issue key (pkg,ver,rev) =
  let prefix = sprintf "%s/%s" pkg ver in
  let new_fix_build = sprintf "%s/%s-%d" pkg ver rev in
  let issue = get_issue key in
  let fix_builds =
    try
      mkstrlist (extract issue [ "fields"; jira_field ])
    with Not_found -> [] in
  let updated_fix_builds =
    `List 
      (List.map 
	(fun x -> `String x)
	(new_fix_build :: 
	  (List.filter
	    (fun b -> 
	      (not (Strings.have_prefix prefix b))) fix_builds))) in
  let body =
    J.to_string
      (`Assoc ["fields", `Assoc [jira_field, updated_fix_builds]]) in
  update_issue key body

(*let _ = fix_issue "SKVT-7213" ("PKG","1.0",3)*)  
ELSE    
let fix_issue key (pkg,ver,rev) = ()
ENDIF


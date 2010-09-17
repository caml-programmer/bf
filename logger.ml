(* Logger *)

open Printf
open System

type logger = {
  port: out_channel;
  start_time: float;
}

let current_time () = Unix.gettimeofday ()

let open_logfile () =
  let logfile =
    Filename.concat
      (Params.get_param "start-dir")
      (Filename.concat
	(Params.get_param "log-dir")
	(Params.get_param "component"))
  in
  create_directory_r (path_directory logfile);
  open_out_gen
    [Open_wronly;Open_append;Open_creat(*;Open_trunc*)]
    0o644 logfile

let open_logger () =
  { port = open_logfile (); start_time = current_time () }
  
let human_timestamp t =
  let nano_sec = 
    int_of_float (10000.0 *. (t -. (floor t))) in
  let tm = Unix.localtime t in
  sprintf "%02d/%02d %02d:%02d:%02d.%04d"
    (tm.Unix.tm_mon+1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
    nano_sec

let log_message ?(low=false) ?key ?logger message =
  let timestamp = current_time () in
  let (starttime,port) =
    match logger with
      | Some l -> l.start_time, l.port
      | None   -> timestamp, open_logfile ()
  in 
  let s =
    match key with
      | None   -> (sprintf "%s> %s\n"      (human_timestamp timestamp) message)
      | Some k -> (sprintf "%s> [%s] %s\n" (human_timestamp timestamp) k message)
  in
  output_string port s; flush port;
  if not low then 
    ( output_string stdout s; flush stdout );
  match logger with
    | Some l ->	flush port
    | None   ->	close_out port

let close_logger logger =
  close_out logger.port

let with_logger proc =
  let logger = open_logger ()
  in proc logger;
  close_logger logger

exception Found_program of string

let with_path s =
  let len = String.length s in
  if len > 2 && String.sub s 0 2 = "./" then
    Filename.concat 
      (Sys.getcwd ()) 
      (String.sub s 2 (len - 2))
  else
  try
    List.iter
      (fun dir ->
	let program = dir ^ "/" ^ s in
	if Sys.file_exists program then
	  raise (Found_program program)
	else ()) System.path_list; s
  with Found_program program -> program

let linearization s =
  let len = String.length s in
  let s = String.sub s 0 len in
  for i=0 to pred len do
    if s.[i] = '\n' || s.[i] = '\r' then
      s.[i] <- ' ';
  done; s

exception Error

let log_error error =
  log_message ~key:"error" error;
  raise Error

let log_command ?(low=false) ?env ?error_handler prog args =
  let out_buf = Buffer.create 256 in
  let err_buf = Buffer.create 256 in
  with_logger
    (fun logger ->
      try
	let program = with_path prog in
	let environment = Shell_sys.create_env () in
	Array.iter
	  (fun s ->
	    (try
	      let (key,value) =
		System.split_env_var s
	      in Shell_sys.set_env_var environment key value
	    with Not_found -> ()))
	  (match env with Some e -> e | None -> Env.component ());
	let cmd =
	  Shell.cmd
	    ~cmdname:program
	    ~environment
	    program args
	in
	let cmd_s = program ^ " " ^ (String.concat " " args) in
	let log_fd =
	  if low then
	    Unix.descr_of_out_channel logger.port
	  else
	    match Params.get_param "log-level" with
	      | "low"  -> Unix.descr_of_out_channel logger.port
	      | "high" -> Unix.descr_of_out_channel stdout
	      | _      -> Unix.descr_of_out_channel stdout
	in
	log_message ~low ~logger (sprintf "run: %s" cmd_s);
	Shell.call
	  ~stdout:(Shell.to_fd log_fd)
	  ~stderr:(Shell.to_fd log_fd) [cmd];
	log_message ~low ~logger (sprintf "success: %s" cmd_s)
      with 
	| Unix.Unix_error(error,name,arg) ->
	    log_error
	      (sprintf "failed: Unix.Unix_error(%s,%s,%s)" 
		(Unix.error_message error)
		name arg)
	| Shell.Subprocess_error errors ->
	    List.iter
	      (fun (cmd,ps) ->
		(match ps with
		  | Unix.WEXITED rc ->
		      (match error_handler with
			  Some f -> f ps
			| None ->
			    log_message ~logger (sprintf "failed: %d" rc);
			    exit rc)
		  | Unix.WSIGNALED n -> 
		      (match error_handler with
			  Some f -> f ps
			| None ->
			    log_message ~logger (sprintf "killed: %d" n);
			    exit n)
		  | Unix.WSTOPPED n ->
		      (match error_handler with
			  Some f -> f ps
			| None ->
			    log_message ~logger (sprintf "stopped: %d" n);
			    exit n)))
	      errors)

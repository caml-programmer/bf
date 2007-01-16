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
      (Params.get_param "log-dir")
      (Params.get_param "component")
  in
  create_directory_r (path_directory logfile);
  open_out_gen
    [Open_wronly;Open_append;Open_creat(*;Open_trunc*)]
    0o644 logfile

let open_logger () =
  { port = open_logfile (); start_time = current_time () }
  
let log_message ?key ?logger message =
  let timestamp = current_time () in
  let (starttime,port) =
    match logger with
      | Some l -> l.start_time, l.port
      | None   -> timestamp, open_logfile ()
  in 
  let s =
    match key with
      | None   -> (sprintf "%f %f> %s\n"      starttime timestamp message)
      | Some k -> (sprintf "%f %f> [%s] %s\n" starttime timestamp k message)
  in
  output_string port s;
  output_string stdout s;
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

let log_command prog args =
  let out_buf = Buffer.create 256 in
  let err_buf = Buffer.create 256 in
  with_logger
    (fun logger ->
      try
	let program = with_path prog in
	let cmd = Shell.cmd ~cmdname:program program args in
	let cmd_s = program ^ " " ^ (String.concat " " args) in
	log_message ~logger (sprintf "run: %s" cmd_s);
	Shell.call
	  ~stdout:(Shell.to_buffer out_buf)
	  ~stderr:(Shell.to_buffer err_buf) [cmd];
	log_message ~logger (sprintf "success: %s" cmd_s)
      with Shell.Subprocess_error errors ->
	List.iter
	(fun (cmd,ps) ->
	  (match ps with
	    | Unix.WEXITED rc ->
		log_message ~logger (sprintf "failed: %d" rc);
		exit rc
	    | Unix.WSIGNALED n -> 
		log_message ~logger (sprintf "killed: %d" n);
		exit n
	    | Unix.WSTOPPED n ->
		log_message ~logger (sprintf "stopped: %d" n);
		exit n))
	errors)

let log_error error =
  log_message ~key:"error" error;
  exit 3

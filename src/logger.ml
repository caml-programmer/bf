(* Logger *)

open Printf
open System

type logger = {
  port: out_channel;
  start_time: float;
}

let need_truncate file =
  if Sys.file_exists file then
    let size =
      (Unix.stat file).Unix.st_size in
    size >= 8388608
  else false
    
let prepare file =
  let dir = Filename.dirname file in
  if dir <> "" && dir <> "." then
    create_directory_r dir;
  if need_truncate file then
    open_out file
  else open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 file

let session_port = (* for common session logfile *)
  lazy (prepare (Params.get_param "session-log"))
;;

let current_time () = Unix.gettimeofday ()

let open_logfile () =
  prepare (Filename.concat
    (Params.get_param "start-dir")
    (Filename.concat
      (Params.get_param "log-dir")
      (Params.get_param "component")))

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
      | None   -> (sprintf "%s(%0.3f)> %s\n"      (human_timestamp timestamp) (timestamp -. starttime) message)
      | Some k -> (sprintf "%s(%0.3f)> [%s] %s\n" (human_timestamp timestamp) (timestamp -. starttime) k message)
  in
  let write p s =
    output_string p s; flush p in
  write (Lazy.force session_port) s;
  write port s;
  if not low then
    ( write stdout s );
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
	else ())
      System.path_list;
    s
  with Found_program program -> program

let linearization s =
  let len = Bytes.length s in
  let s = Bytes.sub s 0 len in
  for i=0 to pred len do
    if Bytes.get s i = '\n' || Bytes.get s i = '\r' then
      Bytes.set s i ' ';
  done; s

exception Error

let log_error error =
  log_message ~key:"error" error;
  raise Error

let log_command ?(low=false) ?env ?error_handler prog args =
  let program = with_path prog in
  let command = program ^ " " ^ (String.concat " " args) in
  with_logger
    (fun logger ->
      try
	let environment = 
	  match env with 
	      Some e -> e 
	    | None -> Env.component () in
	let low =
	  Params.get_param "display-command-logs" = "false" in
	let log_fd =
	  if low then
	    Unix.descr_of_out_channel logger.port
	  else
	    match Params.get_param "log-level" with
	      | "low"  -> Unix.descr_of_out_channel logger.port
	      | "high" -> Unix.descr_of_out_channel stdout
	      | _      -> Unix.descr_of_out_channel stdout in
	log_message ~low ~logger (sprintf "run: %s" command);
	(match Unix.fork () with
	  | 0 ->  (* child *)
	      if log_fd <> Unix.stdout then
		Unix.dup2 log_fd Unix.stdout;
	      Unix.execve program (Array.of_list (program::args)) environment
	  | pid ->
	      let (_,ps) = Unix.waitpid [] pid in
	      if ps = Unix.WEXITED 0 then
		log_message ~low ~logger (sprintf "success: %s" command)
	      else
		(match ps with
		  | Unix.WEXITED rc ->
		      (match error_handler with
			  Some f -> f ps
			| None ->
			    log_message ~logger (sprintf "failed: %d [%s]" rc command);
			    raise Error)
		  | Unix.WSIGNALED n -> 
		      (match error_handler with
			  Some f -> f ps
			| None ->
			    log_message ~logger (sprintf "killed: %d [%s]" n command);
			    raise Error)
		  | Unix.WSTOPPED n ->
		      (match error_handler with
			Some f -> f ps
			| None ->
			    log_message ~logger (sprintf "stopped: %d" n);
			    raise Error)));
      with 
	| Unix.Unix_error(error,name,arg) ->
	    log_error
	      (sprintf "failed: Unix.Unix_error(%s,%s,%s)" 
		(Unix.error_message error)
		name arg))

let run_command prog args =
  let program = with_path prog in
  let command = program ^ " " ^ (String.concat " " args) in
  match Unix.fork () with
    | 0 ->  (* child *)
	Unix.execv program (Array.of_list (program::args))
    | pid ->
	let (_,ps) = Unix.waitpid [] pid in
	(match ps with
	  | Unix.WEXITED rc -> rc
	  | Unix.WSIGNALED n -> 
	      log_message (sprintf "killed: %d [%s]" n command);
	      raise Error
	  | Unix.WSTOPPED n ->
	      log_message (sprintf "stopped: %d" n);
	      raise Error)

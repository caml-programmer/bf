open Getopt
open Printf

(** {1 Program name} *)

let program = Filename.basename Sys.argv.(0)

(** {1 Authors section} *)

let signature = sprintf
  "This program '%s' developed by
   * Masljuk Dmitry <dima@aep.mpei.ac.ru>"
  program


(** {1 Common setting} *)

let version     = "1.0"
let debug       = ref false
let verbose     = ref false
let confdir     = ref "."
let config      = ref (program ^ ".conf")
let pidfile     = ref (program ^ ".pid")
let use_config  = ref false
let daemon_mode = ref false

(** {1 Program setting} *)

let smtp_server   = ref "localhost"
let smtp_port     = ref 25
let to_mail       = ref "sender@localhost"
let to_name       = ref ""
let from_mail     = ref "sender@localhost"
let from_name     = ref "Sender"
let subject       = ref "Message from sender"
let message       = ref ""
let gzip_mode     = ref false
let mimetype      = ref "text/plain; charset=UTF-8"

(** {1 Variable types } *)

exception Parsing_error of string

type variable =
    I of int ref
  | F of float ref
  | B of bool ref
  | S of string ref

let t = function
    I v -> string_of_int !v
  | F v -> string_of_float !v
  | B v -> if !v then "true" else "false"
  | S v -> !v

let once x () = printf "%s\n" x; flush stdout; exit 0

let invt x () = x := not !x

let set_int x v = x := int_of_string v
let set_float x v = x := float_of_string v
let set_string x v = x := v
let set_bool x = function
  | "true"  -> x := true
  | "false" -> x := false
  | _       -> x := false

let set_variable x v =
  match x with
      I z -> set_int z v
    | F z -> set_float z v
    | B z -> set_bool z v
    | S z -> z := v
	
let tab x s d bl br =
  let l = String.length x in
  if l < s then
    let rec sub x v =
      if x = 0 then v
      else sub (x-1) (v^d)
    in sub (s-l) (bl^x^br)
  else x
    
let rec param () =
  let specs =
    let rec create x = function
	[] -> x
      | (c,n,d,v,_,_)::tl ->
	  let l = match v with None -> "none" | Some v -> t v in
	  let s = (sprintf "%s\t-%c, --%s (%s) %s\n" x c
	    (tab n 18 "." "" " ") (tab l 20 "_" "\"" "\" ") d) 
	  in create s tl
    in create "" description
  in printf 
       "Usage: %s <options>\n\nOptions are:\n\n%s\n\n"
       Sys.argv.(0) specs; flush stdout

and help () = param (); exit 0

and mkconfig () =
  let specs =
    let rec create x = function
	[] -> x
      | (c,n,d,v,_,_)::tl ->
	  match v with
	      None -> create x tl
	    | Some v -> create 
		(sprintf "%s%s=  %s\n" x 
		  (tab n 25 " " "" "") (t v)) tl
    in create "" description
  in printf "%s" specs; flush stdout; exit 0

and description  = [
  
  (** {1 Program options} *)
  
  ('s', "smtp-server",   "smtp server",
  Some (S smtp_server), None, Some (set_string smtp_server));

  ('p', "smtp-port",   "smtp port",
  Some (I smtp_port), None, Some (set_int smtp_port));
  
  ('t', "to-mail",   "to mail",
   Some (S to_mail), None, Some (set_string to_mail));

  ('T', "to-name",   "to name",
   Some (S to_name), None, Some (set_string to_name));

  ('f', "from-mail", "from mail",
   Some (S from_mail), None, Some (set_string from_mail));

  ('F', "from-name",   "from name",
   Some (S from_name), None, Some (set_string from_name));

  ('S', "subject",   "message subject",
  Some (S subject), None, Some (set_string subject));

  ('m', "message",   "message file or directory, unless read it from stdin",
  Some (S message), None, Some (set_string message));

  ('g', "gzip-mode", "gzip file before send",
  Some (B gzip_mode), Some (invt gzip_mode), None);

  ('-', "mime-type", "MIME type",
  Some (S mimetype), None, Some (set_string mimetype));


  (** {1 Common options} *)

  ('-', "bg",  "daemon mode",
  Some (B daemon_mode), Some (invt daemon_mode), None);
  
  ('x', "debug",    "inversion of default debug mode",
  Some (B debug), Some (invt debug), None);
  
  ('v', "verbose",  "inversion of default verbose output",
  Some (B verbose), Some (invt verbose), None);
  
  ('V', "version",  "print program version",
  None, Some (once version), None);
  
  ('h', "help",     "print this message and exit",
  None, Some help, None);

  ('-', "mkconfig", "make config from settings",
  None, Some mkconfig, None);
  
  ('D', "confdir",  "directory with config",
  Some (S confdir), None, Some (set_string confdir));
  
  ('-', "config",   "name of configuration file",
  Some (S config), None, Some (set_string config));

  ('-', "pidfile",  "name of pid file",
  Some (S pidfile), None, Some (set_string pidfile));

  ('C', "use-config", "load parameters from config",
  Some (B use_config), Some (invt use_config), None);
  
  ('-', "param",    "print this message and run",
  None, Some param, None);
  
  ('z', "signature","print signature",
  None, Some (once signature), None);
  
];;

let find_variable desc n =
  let z = ref None in
  List.iter 
    (function	
      | (_,name,_,v,_,_) when n = name -> z := v
      | _ -> ()) 
    desc;
  match !z with
      None -> raise Not_found
    | Some v -> v

let parse_confline s =
  let k = Buffer.create 32 in
  let v = Buffer.create 32 in
  let state = ref 0  in
  String.iter 
    (function
      | '\t' | ' ' ->
	  (match !state with
	    | 0 -> ()                         (** read whitespace *)
	    | 1 -> state := 2;                (** key is readed *)
	    | 2 -> ()                         (** read whitespace 2 *)
	    | 3 -> state := 4;                (** value is readed *)
	    | _ -> ()
	  )
      | '=' ->
	  (match !state with
	    | 0 -> raise (Parsing_error s)    (** key is missing *)
	    | 1 -> state := 2;                (** found splitter *)
	    | _ -> ()
	  )
      | c   ->
	  (match !state with
	    | 0 -> state := 1; Buffer.add_char k c (** found key *)
	    | 1 -> Buffer.add_char k c             (** read key *)
	    | 2 -> state := 3; Buffer.add_char v c (** found value *)
	    | 3 -> Buffer.add_char v c             (** read value *)
	    | _ -> ()
	  )) s;
  Buffer.contents k, Buffer.contents v

let load_config desc () = 
  let conf = Filename.concat !confdir !config in
  if !use_config then
    if Sys.file_exists conf then	
      let ch = open_in conf in
      try
	if !debug then printf "loading config\n";
	while true do

	  let (k,v) = 
	    parse_confline (input_line ch)
	  in 		 
	  (try
	    let z = find_variable desc k in
 	    if !debug then
	      eprintf "setting %s %s -> %s\n" k (t z) v;
	    set_variable z v;		   
	  with Not_found -> 
	    if !debug then
	      eprintf "setting %s is not found\n" k);
	  flush stderr;
	  
	done
      with End_of_file -> close_in ch
    else
      ( eprintf "config file: %s is not exists\n" conf; 
      exit 1 )

let specs =
  let rec create x = function
      [] -> x
    | hd::tl ->	
	let (c,n,_,_,a,b) = hd in 
	create (x @ [(c,n,a,b)]) tl
  in create [] description

let daemonization pidfile =
  let ch = open_out pidfile in
  match Unix.fork() with
      0 ->
        ( match Unix.fork() with
            0 ->                
              ignore(Unix.setsid()); 
	      Unix.close Unix.stdin;
              ignore(Unix.openfile "/dev/null" [Unix.O_RDONLY] 0);
              Unix.close Unix.stdout;
	      ignore(Unix.openfile "/dev/null" [Unix.O_WRONLY] 0);
              Unix.close Unix.stderr;
              ignore(Unix.openfile "/dev/null" [Unix.O_WRONLY] 0);
              Unix.chdir "/";
              let pid = Unix.getpid () in
              output_string ch (string_of_int pid);
              output_string ch "\n";
              close_out ch;
          | _ ->
              exit 0 )
    | _ ->
        exit 0
	  
let _ =
  try  
    parse_cmdline specs print_endline;
    load_config description ();
    if !daemon_mode then
      daemonization !pidfile
  with e -> 
    prerr_endline (Printexc.to_string e);
    param (); exit 0

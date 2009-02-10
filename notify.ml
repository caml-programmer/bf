(* Notification *)

open Printf
open Netchannels

let error_message s =
  prerr_endline s

let message s =
  print_endline s

let string_of_exn = function
  | Netsmtp.Transient_error (err,msg) ->
      sprintf "Transient Error: %d, %s" err msg
  | Netsmtp.Permanent_error (err,msg) ->
      sprintf "Permanent Error: %d, %s" err msg
  | Invalid_argument s ->
      sprintf "Invalid_argument: %s" s
  | exn ->
      Printexc.to_string exn

let day = 
  [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |];;
let month =
  [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";  "Jul"; "Aug" ; "Sep" ; "Oct" ; "Nov" ; "Dec" |];;

let make_date () =
  let tm = Unix.gmtime (Unix.time ()) in
  sprintf "%s, %d %s %d %02d:%02d:%02d 0000 (GMT)"
    day.(tm.Unix.tm_wday)
    tm.Unix.tm_mday
    month.(tm.Unix.tm_mon)
    (1900+tm.Unix.tm_year)
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let make_base64 s =
  "=?UTF-8?B?" ^ (Netencoding.Base64.encode s) ^ "?="

let boundary =
  "------------060501060303010105060205"
    
let make_header 
  ?(from_name="bf") 
  ?(from_mail="bf@notify")
  ?(subject="bf message") 
  ~to_mail b =
  let add = Buffer.add_string b in
  add (sprintf "From: %s <%s>\n" from_name from_mail);
  add (sprintf "To: %s\n" to_mail);    
  add (sprintf "Subject: %s\n" (make_base64 subject));
  add (sprintf "Date: %s\n" (make_date()));
  add (sprintf "User-agent: %s\n" "bf notify module");
  add (sprintf "MIME-version: %s\n" "1.0");
  add (sprintf "Content-type: multipart/mixed; boundary=\"%s\"\n" boundary)

class input_contents ~header ~mimetype ~contents : in_obj_channel =
object (self)
  val mutable chunks = []
  val mutable str = ""
  val mutable str_len = 0
  val mutable str_pos = 0
  val mutable closed = false

  initializer
    self # set_chunks ();
    self # check_chunk ()

  method private set_chunks () =
    let add s = chunks <- s::chunks in
    add header;
    add "\n";
    add (sprintf "This is a multi-part message in MIME format.\n");
    add (sprintf "--%s\n" boundary);
    add (sprintf "Content-type: %s\n" mimetype);
    add (sprintf "Content-Transfer-Encoding: %s\n\n" "base64");
    List.iter
      (fun content ->
	add (Netencoding.Base64.encode ~linelength:76 content))
      contents;
    add (sprintf "--%s--\n" boundary);
    chunks <- List.rev chunks

  method private set_next_chunk () =
    match chunks with
      | hd::tl ->
	  chunks <- tl;
	  str <- hd;
	  str_len <- String.length hd;
	  str_pos <- 0;
	  self # check_chunk ();
      | [] -> raise End_of_file

  method private check_chunk () =
    if str_pos >= str_len then
      self # set_next_chunk ()

  method private complain_closed() =
    raise Closed_channel

  method input buf pos len =
    if closed then self # complain_closed();
    if pos < 0 || len < 0 || pos+len > String.length buf then
      invalid_arg "input";

    let n = min len (str_len - str_pos) in
    String.blit str str_pos buf pos n;
    
    str_pos <- str_pos + n;

    if n=0 && len>0 then
      (self # check_chunk (); 0)
    else n

  method really_input buf pos len =
    if closed then self # complain_closed();
    if pos < 0 || len < 0 || pos+len > String.length buf then
      invalid_arg "really_input";

    let n = self # input buf pos len in
    if n <> len && chunks = [] then raise End_of_file;
    ()

  method input_char() =
    if closed then self # complain_closed();
    self # check_chunk ();
    let c = str.[ str_pos ] in
    str_pos <- str_pos + 1;
    c

  method input_line() =
    if closed then self # complain_closed();
    self # check_chunk ();
    try
      let k = String.index_from str str_pos '\n' in
      let line = String.sub str str_pos (k - str_pos) in
      str_pos <- k+1;
      line
    with
	Not_found ->
	  if str_pos >= str_len then raise End_of_file;
	  (* Implicitly add linefeed at the end of the file: *)
	  let line = String.sub str str_pos (str_len - str_pos) in
	  str_pos <- str_len;
	  line

  method input_byte() =
    Char.code (self # input_char())

  method close_in() =
    if closed then self # complain_closed();
    str <- "";
    closed <- true;

  method pos_in = 
    if closed then self # complain_closed();
    str_pos

end

let send_message
  ?(smtp_server=(Params.get_param "smtp-server"))
  ?(smtp_port=(int_of_string (Params.get_param "smtp-port")))
  ?(from_name=(Params.get_param "smtp-from-name"))
  ?(from_mail=(Params.get_param "smtp-from-mail"))
  ?(subject=(Params.get_param "smtp-subject"))
  ?(mimetype="text/plain; charset=UTF-8")
  ~contents to_mail =
  message (sprintf "Connect to %s:%d" smtp_server smtp_port);
  message (sprintf "Subject: %s" subject);
  try
    let host = Unix.gethostbyname smtp_server in
    let (in_channel,out_channel) =
      let rec make () =
	(try
	  Unix.open_connection
	    (Unix.ADDR_INET(host.Unix.h_addr_list.(0),smtp_port))
	with Unix.Unix_error(Unix.EINTR,_,_) ->
	  message "wait one second to connect";
	  Unix.sleep 1; make ())
      in make ()
    in
	  
    let (in_port,out_port) =
      new Netchannels.input_channel in_channel,
      new Netchannels.output_channel out_channel in
    
    let smtp = new Netsmtp.client in_port out_port in

    List.iter message (smtp # helo ());
    smtp # mail from_mail;
    smtp # rcpt to_mail;

    let buffer = Buffer.create 1024 in

    make_header 
      ~from_name
      ~from_mail
      ~subject
      ~to_mail buffer;

    let stream =
      new input_contents
	~mimetype
	~header:(Buffer.contents buffer)
	~contents in

    message "Send message";
    message (Buffer.contents buffer);
    smtp # data stream;

    message "Send quit";
    smtp # quit ()

  with exn ->
    error_message (string_of_exn exn);
    exit 2

let package_build_message ~host ~location ~pkgname ~storage =
  sprintf "Package %s/%s built on %s and copied to %s\n"
    location pkgname host storage

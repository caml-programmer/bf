(* Notification *)

open Printf

exception Host_not_found of string

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
  "=?UTF-8?B?" ^ (Bytes.to_string (Base64.encode (Bytes.of_string s))) ^ "?="

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

exception Smtp_error of string

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
    let host = 
      try
	Unix.gethostbyname smtp_server 
      with Not_found -> raise (Host_not_found smtp_server)
    in
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

    let smtp = new Netsmtp.client in_channel out_channel in

    List.iter message (smtp # helo ());
    smtp # mail from_mail;
    smtp # rcpt to_mail;

    let buffer = Buffer.create 1024 in
    make_header 
      ~from_name
      ~from_mail
      ~subject
      ~to_mail buffer;

    let write_data () =
      let file =
	sprintf "/tmp/notify.msg.%d" (Random.int 10000) in
      let ch = open_out file in
      let out = output_string ch in      
      out (Buffer.contents buffer);
      out "\n";
      out (sprintf "This is a multi-part message in MIME format.\n");
      out (sprintf "--%s\n" boundary);
      out (sprintf "Content-type: %s\n" mimetype);
      out (sprintf "Content-Transfer-Encoding: %s\n\n" "base64");
      List.iter
	(fun content ->
	  out (Bytes.to_string (Base64.encode ~linelength:76 (Bytes.of_string content))))
	contents;
      out (sprintf "--%s--\n" boundary);
      close_out ch;
      let i = open_in file in
      smtp # data i;
      close_in i;
      Unix.unlink file in

    message "Send message";
    message (Buffer.contents buffer);
    write_data ();
    message "Send quit";
    smtp # quit ()

  with exn ->
    raise (Smtp_error (string_of_exn exn))

let package_build_message ~host ~location ~pkgname ~storage =
  sprintf "Package %s/%s was build on %s and copied to %s\n"
    location pkgname host storage

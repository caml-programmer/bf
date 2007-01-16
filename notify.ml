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

let send_message
  ?(smtp_server=(Params.get_param "smtp-server"))
  ?(smtp_port=(int_of_string (Params.get_param "smtp-port")))
  ?(from_name=(Params.get_param "smtp-from-name"))
  ?(from_mail=(Params.get_param "smtp-from-mail"))
  ?(subject=(Params.get_param "smtp-subject"))
  ?(mimetype="text/plain; charset=UTF-8")
  ~content to_mail =
  message (sprintf "Connect to %s:%d" smtp_server smtp_port);
  message (sprintf "Subject: %s" subject);
  try
    let host = Unix.gethostbyname smtp_server in    
    let (in_channel,out_channel) =
      Unix.open_connection
	(Unix.ADDR_INET(host.Unix.h_addr_list.(0),smtp_port)) in
    
    let (in_port,out_port) =
      new Netchannels.input_channel in_channel,
      new Netchannels.output_channel out_channel in
    
    let buffer = Buffer.create 1024 in
    let add = Buffer.add_string buffer in
    let smtp = new Netsmtp.client in_port out_port in

    List.iter message (smtp # helo ());
    smtp # mail from_mail;
    smtp # rcpt to_mail;
    
    make_header 
      ~from_name
      ~from_mail
      ~subject
      ~to_mail buffer;

    add "\n";
    add (sprintf "This is a multi-part message in MIME format.\n");
    add (sprintf "--%s\n" boundary);
    add (sprintf "Content-type: %s\n" mimetype);
    add (sprintf "Content-Transfer-Encoding: %s\n\n" "base64");
    Buffer.add_string buffer
      (Netencoding.Base64.encode ~linelength:76 content);
    add (sprintf "--%s--\n" boundary);

    message "Send message";
    message (Buffer.contents buffer);

    smtp # data 
      (new input_string (Buffer.contents buffer));

    message "Send quit";
    smtp # quit ()

  with exn ->
    error_message (string_of_exn exn);
    exit 2

let package_build_message ~host ~location ~pkgname ~storage =
  sprintf "Package %s/%s built on %s and copied to %s\n"
    location pkgname host storage

(* HTTP client *)

open Printf

exception Error of string
exception Invalid_hostname of string
exception Host_not_found of string
exception Unknown_transfer_encoding of string
exception Transfer_encoding_not_found
exception Big_response_body

(* utils *)
 
let hostbyname s =
  try
    (Unix.gethostbyname s).Unix.h_addr_list.(0)
  with Not_found -> raise (Host_not_found s)

let nodename s =
  try
    String.sub s 0 (String.index s '.')
  with Not_found -> s

let parse_header s =
  let pos = String.index s ':' in
  String.lowercase 
    (String.sub s 0 pos),
  String.sub s (pos + 2)
    (String.length s - pos - 3)
      
let parse_status s =
  let p1 = String.index s ' ' in
  let p2 = String.index_from s (succ p1) ' ' in
  String.sub s 0 p1,
  String.sub s (succ p1) (p2 - p1 - 1),
  String.sub s p2 (String.length s - p2 - 1)

let nocarry s =
  let l = String.length s in
  String.sub s 0 (pred l)

exception Cannot_parse_chunksize of string * string

let chunksize s =
  let x = ref 0 in
  (try
    Scanf.sscanf s "%x" (fun n -> x := n);
  with exn ->
    raise (Cannot_parse_chunksize (s, (Printexc.to_string exn))));
  !x

(* readers *)

let read_header i =
  let rec make acc =
    let s = input_line i in
    if s = "\r" || s = "" then
      acc
    else
      make ((parse_header s)::acc)
  in make []

let read_status i =
  parse_status (input_line i)
   
let read_chunked i =
  let b = Buffer.create 32 in
  (try
    while true do
      let size = chunksize (nocarry (input_line i)) in
      (*Printf.printf "chunk length: (%d)\n%!" size;*)
      if size = 0 then
	raise Exit;
      for k=0 to pred size do
	Buffer.add_char b (input_char i)
      done;
      ignore(input_line i);
    done
  with Exit -> ());
  Buffer.contents b

let read_body ?size i =
  match size with
    | Some n ->
	let s = String.create n in
	for j=0 to pred n do
	  String.unsafe_set s j (input_char i);
	done; s
    | None ->
	read_chunked i

let read_unlimited_body i =
  let b = Buffer.create 32 in
  let mb = 1024 * 1024 in
  let rec read n =
    if n > 10 * mb then
      raise Big_response_body
    else
      begin
	Buffer.add_char b (input_char i);
	read (succ n)
      end
  in 
  (try read 0 with End_of_file -> ());
  Buffer.contents b

(* request *)
   
let call_request log (i,o) query =
  (try
    log (sprintf ">> %s\n%!" query);
    output_string o query;
    flush o;

    let (proto,code,status) =
      read_status i in

    log (sprintf "<< %s %s %s\n" proto code status);

    let header =
      read_header i in

    List.iter 
      (fun (k,v) -> 
	log (sprintf "<< %s: %s\n" k v))
      header;

    let body =
      try
	let size =
	  int_of_string (List.assoc "content-length" header) in
	read_body ~size i
      with Not_found ->
	(match 
	  try 
	    Some (List.assoc "transfer-encoding" header)
	  with Not_found -> None
	with
	  | Some "chunked" -> read_chunked i
	  | Some s -> 
	      raise (Unknown_transfer_encoding s)
	  | None ->
	      read_unlimited_body i)
    in
    log (sprintf "<< (body (length %d))\n%!" (String.length body));
    (code,header,body)
  with
    | Unix.Unix_error (error,_,_) ->
	raise (Error (Printf.sprintf "%s while send query (%s)" (Unix.error_message error) query))
    | exn -> raise exn)

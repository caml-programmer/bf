open Logger
open Printf

let key_rex = Pcre.regexp ":"
let space_rex = Pcre.regexp "\\s+"

let non_empty l =
  List.filter ((<>) "") l

let parse add s =
  match Pcre.split ~rex:key_rex s with
    | [key;values] ->
	add ((Strings.drop_spaces key), (non_empty (Pcre.split ~rex:space_rex values)))
    | l ->
	List.iter print_endline l

let load ?(log=false) () =
  let file = ".bf-synonyms" in
  if Sys.file_exists file then
    begin
      if log then
	log_message (sprintf "loading %s\n%!" file);
      let data = ref [] in
      let ch = open_in file in
      try
	while true do
	  let add (name,emails) =
	    let non_empty_emails = 
	      List.filter (fun e -> e <> "") emails in
	    (* log_message
	       (sprintf "add synonim: name(%s), emails(%s)" name (String.concat "," non_empty_emails)); *)
	    data := (name,non_empty_emails)::!data in
	  parse add (input_line ch)
	done; []
      with End_of_file ->
	close_in ch;
	!data
    end
  else []

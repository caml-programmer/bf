open Logger
open Printf

let rex = Pcre.regexp "\\s+"

let is_email s =
  try
    ignore(String.index s '@');
    true
  with Not_found -> false

let email_splitter l =
  let rec split (name,emails) = function
    | [] -> (name,emails)
    | hd::tl ->
	if is_email hd then
	  split ([],hd::emails) tl
	else
	  split (hd::name,emails) tl in
  let x = split ([],[]) (List.rev l) in
  String.concat " " (fst x), (snd x)

let parse add s =
  match Pcre.split ~rex s with
    | name::emails when name <> "" ->
	add (email_splitter (name::emails))
    | _::name::emails ->
	add (email_splitter (name::emails))
    | _ -> ()

let load () =
  let file = ".bf-synonyms" in
  if Sys.file_exists file then
    begin
      log_message (sprintf "load %s\n%!" file);
      let data = ref [] in
      let ch = open_in file in
      try
	while true do
	  let add (name,emails) =
	    let non_empty_emails = 
	      List.filter (fun e -> e <> "") emails in
	    log_message
	      (sprintf "add synonim: name(%s), emails(%s)" name (String.concat "," non_empty_emails));
	    data := (name,non_empty_emails)::!data in
	  parse add (input_line ch)
	done; []
      with End_of_file ->
	close_in ch;
	!data
    end
  else []

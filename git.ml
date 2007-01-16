open Types
open Logger
open System

let git_clone url =
  log_command "git" ["clone";"-n";"-q";url]

let git_pull url =
  log_command "git" ["pull";url]

let git_checkout ?key ?files () =
  match (key,files) with
    | None, None ->
	log_command "git "["checkout"]
    | None, Some fl ->
	log_command "git" ("checkout"::fl)
    | Some k, None ->
	log_command "git" ["checkout";k]
    | Some k, Some fl ->
	log_command "git" ("checkout"::k::fl)

let git_branch () =
  let (ch,out,err) = Unix.open_process_full "git branch -r" (Unix.environment ()) in
  let rec read acc =
    try
      let s = input_line ch in
      let l = String.length s in
      if l > 2 then
	read (acc @ [String.sub s 2 (l-2)])
      else read acc
    with End_of_file ->
      let error = string_of_channel err in
      match Unix.close_process_full (ch,out,err) with
	| Unix.WEXITED 0 -> acc
	| _ -> log_error error
  in read []
  
let git_clean () =
  log_command "git" ["clean";"-d";"-x"]

let git_diff ?tag () =
  let cmd =
    match tag with
      | None   -> "git diff"
      | Some t -> "git diff " ^ t
  in
  let ch = Unix.open_process_in cmd in
  try 
    ignore (input_line ch);
    close_in ch; true 
  with End_of_file ->
    close_in ch; false

let git_tag_list () =
  let (ch,out,err) = Unix.open_process_full "git tag -l" (Unix.environment ()) in
  let rec read acc =
    try
      read (acc @ [(input_line ch)])
    with End_of_file ->
      let error = string_of_channel err in
      match Unix.close_process_full (ch,out,err) with
	| Unix.WEXITED 0 -> acc
	| _ -> log_error error
  in read []

let git_branch_status component = (* TODO: origin handling *)
  let (ch,out,err) = Unix.open_process_full "git branch" (Unix.environment ()) in
  let rec read acc =
    try
      let s = input_line ch in
      let l = String.length s in
      if l > 2 && s.[0] = '*' then
	read (acc @ [String.sub s 2 (l-2)])
      else read acc
    with End_of_file ->
      let error = string_of_channel err in
      match Unix.close_process_full (ch,out,err) with
	| Unix.WEXITED 0 -> acc
	| _ -> log_error error
  in match read [] with
    | []     -> Not_exists
    | hd::tl ->
	(match component.label with
	  | Branch m ->
	      if m = hd then
		Be_set
	      else
		Not_exists
	  | _ -> Be_set)
       
let git_tag_status component =
  match component.label with
    | Current -> Be_set
    | Branch _ -> Be_set
    | Tag m ->	
	let tags =
	  List.filter
	    (fun tag -> tag = m) (git_tag_list ())
	in
	match tags with
	  | [] -> Not_exists
	  | hd::_ ->
	      if git_diff ~tag:hd () then
		Exists
	      else
		Be_set

let git_key_status component =
  match component.label with
    | Current -> Be_set
    | Tag _ -> git_tag_status component
    | Branch _ -> git_branch_status component

let git_tree_status component =
  let cur = Sys.getcwd () in
  if not (System.is_directory component.name) then
    Not_exists
  else
    begin
      Sys.chdir component.name;
      let status =
	git_diff ()
      in Sys.chdir cur;
      if status then 
	Exists
      else Be_set
    end

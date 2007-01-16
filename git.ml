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
	| _ -> log_error error; []
  in read []
  
let git_clean () =
  log_command "git" ["clean";"-d";"-x"]


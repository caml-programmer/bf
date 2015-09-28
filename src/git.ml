open System
open Printf
open Logger
open Output

type tag_status =
  | Tag_already_exists
  | Tag_created
  | Tag_creation_problem
       
let env = Env.system ();;

let git_version =
  match read_lines "git version" with
    | s::_ ->
	(match Str.split (Str.regexp " ") s with
	  | "git"::"version"::version::_ ->
	      List.map int_of_string
		(Str.split (Str.regexp "\\.") version)
	  | _ -> [])
    | _ -> []

let no_edit =
  match git_version with
    | 1::7::1::_ -> []
    | _          -> ["--no-edit"]

let git_init () =
  log_command ~env "git" ["init"]

let git_clone url name =
  log_command ~env "git" ["clone";"-n";"-q";url;name]

let clone ?(branch="") ?(depth=0) url =
  let branch_str = if (String.length branch) != 0 then (" -b "^branch) else "" in
  let depth_str = match depth with 0 -> "" | depth -> (" --depth "^(string_of_int depth)) in
  Cmd.command_log ("git clone "^url^branch_str^depth_str)

let git_add name =
  log_command ~env "git" ["add";name]

let git_commit ?(empty=false) msg =
  if empty then
    log_command ~env "git" ["commit";"--allow-empty";"-m";msg]
  else
    log_command ~env "git" ["commit";"-m";msg]
   
let git_fetch ?refspec ?(tags=false) () =
  let opts = if tags then ["-q";"--tags"] else ["-q"] in
  match refspec with
    | Some spec -> ignore(run_command "git" (["fetch"] @ opts @ [spec]))
    | None -> ignore(run_command "git" (["fetch"] @ opts))

let git_merge remote =
  log_command ~env "git" ["merge";remote]

let git_pull ?(force=false) ?refspec url =
  match refspec with
    | Some spec ->
	let opts = if force then ["--force"] else [] in
	log_command ~env "git" (["pull"] @ no_edit @ opts @ [url;spec])
    | None ->
	let opts = if force then ["--force"] else [] in
	log_command ~env "git" (["pull"] @ no_edit @ opts @ [url])

let git_push ?(tags=false) ?refspec url =
  match refspec with
    | Some spec -> 
	let opts = if tags then ["--tags"] else [] in
	log_command ~env "git" (["push"] @ opts @ [url;spec])
    | None ->
	let opts = if tags then ["--tags"] else [] in
	log_command ~env "git" (["push"] @ opts @ [url])

let git_remote_update spec =
  log_command ~env "git" ["remote";"update";spec]
       
let git_make_tag tag =
  let state = ref Tag_created in
  let error_handler ps =
    match ps with
      | Unix.WEXITED 128 ->
	  log_message (sprintf "tag %s already exists" tag);
	  state := Tag_already_exists
      | _ ->
	  log_message (sprintf "tag %s creation problem" tag);
	  state := Tag_creation_problem
  in log_command ~env
       ~error_handler "git" ["tag";"-a";"-m";tag;tag];
  !state

let git_log ?(pack=None) ?(diff=false) ?(since=None) tag_a tag_b =
  let file =
    match pack with
      | None -> ""
      | Some s -> " " ^ s
  in
  let fmt = "%h %ci %ae : %s" in (* medium *)
  let cmd =
    match since,diff with
      | Some s,true ->
	  sprintf "git log --pretty=format:\"%s\" --since='%s' -p %s%s" fmt s tag_a file
      | None,true ->
	  sprintf "git log --pretty=format:\"%s\" -p '%s'..'%s'%s" fmt tag_a tag_b file
      | Some s,false ->
	  sprintf "git log --pretty=format:\"%s\" --since='%s' %s%s" fmt s tag_a file
      | None,false ->
	  sprintf "git log --pretty=format:\"%s\" '%s'..'%s'%s" fmt tag_a tag_b file
  in
  let chunks = ref [] in
  let buf = Buffer.create 64 in
  let ch = Unix.open_process_in cmd in
  (try
    while true do
      let s = input_line ch in
      if Buffer.length buf + String.length s >= (Sys.max_string_length / 2) then
	begin
	  chunks := (Buffer.contents buf)::!chunks;
	  Buffer.clear buf;
	end;
      Buffer.add_string buf "  ";
      Buffer.add_string buf s;
      Buffer.add_string buf "\n";
    done; ignore(Unix.close_process_in ch)
  with End_of_file ->
    close_in ch);
  chunks := (Buffer.contents buf)::!chunks;
  List.rev !chunks

let git_checkout ?(low=false) ?(force=false) ?branch ?(modify=false) ?(track=false) ?key ?files () =
  let args = ref ["checkout";"-q"] in
  let add arg = args := !args @ [arg] in
  if force then add "-f";
  if track then add "--track";
  (match branch with Some b -> add "-b"; add b | None -> ());
  if modify then add "-m";
  (match key   with Some k -> add k | None -> ());
  (match files with Some l -> List.iter add l | None -> ());
  log_command ~low ~env "git" !args

let checkout label =
  Cmd.command_log ("git checkout "^label)

let ls_files () =
  let (_,output,_) = Cmd.command "git ls-files" in
  output
		  
let git_branch ?(filter=(fun _ -> true)) ?(raw_filter=(fun _ -> true)) ?(remote=false) () =
  let remotes_cleaner s =
    let a = "remotes/origin/" in
    let b = "remotes/" in
    let al = String.length a in
    let bl = String.length b in
    let sl = String.length s in
    if sl > al && String.sub s 0 al = a then
      String.sub s bl (sl - bl)
    else s
  in
  let branch_cleaner s =
    remotes_cleaner 
      (try
	let pos = String.index s '>' in
	let len = pos - 2 in
	if len > 0 then
	  String.sub s 0 len
	else s
      with Not_found -> s)
  in
  try
    List.filter filter
      (List.map branch_cleaner
	(List.map
	  (fun s -> String.sub s 2 ((String.length s) - 2))
	  (read_lines ~env
	    ~filter:(fun s -> String.length s > 2 && raw_filter s && s <> "* (no branch)")
	    (if remote then "git branch -r" else "git branch"))))
  with System.Error s -> log_error s

exception Unfinished_git_push_cycle
 
let git_push_cycle ~tags ~refspec url depth =
  let lb = git_branch () in
  let rec make depth =
    if depth <= 0 then
      raise Unfinished_git_push_cycle
    else
      try
	(match refspec with
	  | Some refspec ->
	      git_push ~tags ~refspec url
	  | None -> 
	      git_push ~tags url)
      with Logger.Error ->
	log_message "git-push cycle: waiting 1 second";
	Unix.sleep 1;
	List.iter 
	  (fun b ->
	    git_checkout ~key:b ~force:true ();
	    git_pull ~refspec:b url)
	  lb;
	make (pred depth)
  in make depth
       
let git_create_branch ?(start=None) s =
  match start with
    | Some start ->
	log_command ~env "git" ["branch";s;start]
    | None ->
	log_command ~env "git" ["branch";s]
	  
let strip_branch_prefix branch =
  let len = String.length branch in
  let pos = String.index branch '/' in
  if len - pos > 1 then
    String.sub branch (succ pos) (len - pos - 1)
  else raise Not_found

let git_track remote_branch =
  try
    let local = strip_branch_prefix remote_branch in
    log_command ~env "git"
      ["checkout";"-f";"-q";"--track";"-b";local;remote_branch]
  with Not_found -> ()
  
let git_current_branch () =
  let current =
    git_branch
      ~raw_filter:(fun s -> s.[0] = '*')
      ~remote:false ()
  in match current with
    | hd::[] -> Some hd
    | _      -> None

let git_track_new_branches () =
  let rb = git_branch ~remote:true ~filter:(fun s -> (strip_branch_prefix s) <> "HEAD") () in
  let lb = git_branch ~remote:false () in
  List.iter
    (fun b ->
      if not (List.mem (strip_branch_prefix b) lb) then
	git_track b) rb

let git_clean () =
  log_command ~env "git" ["clean";"-d";"-x";"-f"]

let git_diff ?(ignore=[]) ?key () =
  let cmd =
    match key with
      | None   -> "git diff --name-status"
      | Some k -> "git diff --name-status " ^ k ^ " --"
  in read_lines
       ~env
       ~filter:(fun s -> not
	 (List.mem
	   (String.sub s 2
	     (String.length s - 2)) ignore))
       cmd

let git_tag_list () =
  try
    read_lines ~env "git tag -l"
  with System.Error s -> log_error s

let git_key_list () =
  (git_branch ()) @ (git_branch ~remote:true ()) @ (git_tag_list ())

let git_drop_tag tag =
  Call.run "git" ["tag";"-d";tag]

exception Bad_tag_date of string

let resolve_tag_date tag =
  let (raw,_) =
    Call.read "git"
      ["log";"-1";"--date=short";"--pretty=format:%ad";tag] in
  let len = String.length raw in
  match
    List.map int_of_string
      (Strings.split '-'
	(if raw.[len-1] = '\n' then 
	  String.sub raw 0 (pred len)
	else raw))
  with [y;m;d] -> (y,m,d)
    | _ -> raise (Bad_tag_date raw)


exception Key_not_found of string

let git_check_key l tag =
  let rex = Pcre.regexp "HEAD" in
  if not (List.mem tag l) && not (Pcre.pmatch ~rex tag) then
    raise (Key_not_found tag)

let git_changes key_a key_b =
  let cmd =
    sprintf "git diff --name-status '%s' '%s' --" key_a key_b in
  try
    read_lines ~env cmd
  with exn ->
    let l = git_key_list () in
    git_check_key l key_a;
    git_check_key l key_b;
    raise exn

let git_changed key_a key_b =
  git_changes key_a key_b <> []

let git_diff_view ~tag_a ~tag_b =
  let cmd =
    sprintf "git diff -M %s %s" tag_a tag_b in
  let buf = Buffer.create 64 in
  let ch = Unix.open_process_in cmd in
  (try
    while true do
      Buffer.add_string buf (input_line ch);
      Buffer.add_string buf "\n";
    done; ignore(Unix.close_process_in ch)
  with End_of_file ->
    close_in ch);
  Buffer.contents buf

let git_status () =
  try
    let lines = read_lines ~env ~ignore_error:true "git status --ignored" in
    if 
      List.length lines = 2 && List.nth lines 1 = "nothing to commit (working directory clean)"
    then [] else lines
  with System.Error s -> log_error s


exception Invalid_url
exception Found_component of string
exception Component_not_found of string

let check_component_url url name =
  let cmd = 
    sprintf "git ls-remote %s" 
      (Filename.concat url name) in
  let ch = Unix.open_process_in cmd in
  let check () =
    match Unix.close_process_in ch with
      | Unix.WEXITED 0 ->
	  raise (Found_component url)
      | _ -> ()
  in
  (try
    while true do
      ignore(input_line ch);
    done; check ()
  with End_of_file -> check ())



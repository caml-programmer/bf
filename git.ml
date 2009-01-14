open Types
open Logger
open System
open Printf

let git_clone url name =
  log_command "git" ["clone";"-n";"-q";url;name]

let git_pull ?refspec url =
  match refspec with
    | Some spec -> log_command "git" ["pull";url;spec]
    | None -> log_command "git" ["pull";url]

let git_push ?refspec url =
  match refspec with
    | Some spec -> log_command "git" ["push";url;spec]
    | None -> log_command "git" ["push";url]

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
  in log_command
       ~error_handler "git" ["tag";"-a";"-m";tag;tag];
  !state

let git_log ?(diff=false) tag_a tag_b =
  let cmd =
    if diff then
      sprintf "git log -p '%s'..'%s'" tag_a tag_b 
    else
      sprintf "git log '%s'..'%s'" tag_a tag_b
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
      Buffer.add_string buf s;
      Buffer.add_string buf "\n";
    done; close_in ch
  with End_of_file ->
    close_in ch);
  chunks := (Buffer.contents buf)::!chunks;
  List.rev !chunks

let git_checkout
  ?(force=false)
  ?branch
  ?(modify=false)
  ?(track=false) ?key ?files () =
  let args = ref ["checkout"] in
  let add arg = args := !args @ [arg] in
  if force then add "-f";
  if track then add "--track";
  (match branch with Some b -> add "-b"; add b | None -> ());
  if modify then add "-m";
  (match key   with Some k -> add k | None -> ());
  (match files with Some l -> List.iter add l | None -> ());
  log_command "git" !args

let git_branch ?(filter=(fun _ -> true)) ?(remote=false) () =
  try
    List.map
      (fun s -> String.sub s 2 ((String.length s) - 2))
      (read_lines
	~filter:(fun s -> String.length s > 2 && filter s && s <> "* (no branch)")
	(if remote then "git branch -r" else "git branch"))
  with System.Error s -> log_error s

let strip_branch_prefix branch =
  let len = String.length branch in
  let pos = String.index branch '/' in
  if len - pos > 1 then
    String.sub branch (succ pos) (len - pos - 1)
  else raise Not_found

let git_track remote_branch =
  try
    let local = strip_branch_prefix remote_branch in
    log_command "git"
      ["checkout";"-f";"-q";"--track";"-b";local;remote_branch]
  with Not_found -> ()
  
let git_current_branch () =
  let current =
    git_branch
      ~filter:(fun s -> s.[0] = '*')
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
  log_command "git" ["clean";"-d";"-x";"-f"]

let git_diff ?(ignore=[]) ?key () =
  let cmd =
    match key with
      | None   -> "git diff --name-status"
      | Some k -> "git diff --name-status " ^ k
  in read_lines
       ~filter:(fun s -> not
	 (List.mem
	   (String.sub s 2
	     (String.length s - 2)) ignore))
       cmd

let git_diff_view ~tag_a ~tag_b =
  let cmd =
    sprintf "git diff %s %s" tag_a tag_b in
  let buf = Buffer.create 64 in
  let ch = Unix.open_process_in cmd in
  (try
    while true do
      Buffer.add_string buf (input_line ch);
      Buffer.add_string buf "\n";
    done; close_in ch
  with End_of_file ->
    close_in ch);
  Buffer.contents buf

let git_tag_list () =
  try
    read_lines "git tag -l"
  with System.Error s -> log_error s

let git_status () =
  try
    let lines = read_lines ~ignore_error:true "git status" in
    if 
      List.length lines = 2 && List.nth lines 1 = "nothing to commit (working directory clean)"
    then [] else lines
  with System.Error s -> log_error s

let git_worktree_status ~strict component =
  let ignore =
    if Sys.file_exists ".bf-ignore" then
      begin
	let ch = open_in ".bf-ignore" in
	let acc = list_of_channel ch in
	close_in ch; acc
      end
    else []
  in  
  let worktree_changes =
    if not strict then [] else git_status () in
  let source_changes =
    match component.label with
      | Current ->
	  git_diff ~ignore ()
      | Tag key ->
	  git_diff ~ignore ~key ()
      | Branch key ->
	  git_diff ~ignore ~key ()
  in
  if source_changes = [] && worktree_changes = [] then
    Tree_prepared
  else
    Tree_changed (source_changes @ worktree_changes)

let git_tag_status ~strict component =
  match component.label with
    | Current  -> assert false
    | Branch _ -> assert false
    | Tag m ->
	let tags = git_tag_list () in
	if not (List.mem m tags) then
	  Tree_exists_with_other_key "unknown"
	else
	  Tree_exists_with_given_key 
	    (git_worktree_status ~strict component)

let git_branch_status ~strict component =
  match component.label with
    | Current  -> assert false
    | Tag _ -> assert false
    | Branch m ->
	let current = git_current_branch () in
	let branches = git_branch ~remote:false () in
	if not (List.mem m branches) then
	  Tree_exists_with_other_key "unknown"
	else
	  match git_current_branch () with
	    | Some cur ->
		if cur = m then
		  Tree_exists_with_given_key (git_worktree_status ~strict component)
		else
		  Tree_exists_with_other_key cur
	    | None ->
		Tree_exists_with_other_key "unknown"

let git_key_status ~strict component =
  match component.label with
    | Current  -> Tree_exists_with_given_key (git_worktree_status ~strict component)
    | Tag _    -> git_tag_status ~strict component
    | Branch _ -> git_branch_status ~strict component

let git_component_status ~strict component =
  let cur = Sys.getcwd () in
  if not (System.is_directory component.name) then
    Tree_not_exists
  else
    begin
      Sys.chdir component.name;
      let status =
	git_key_status ~strict component in
      Sys.chdir cur;
      status
    end



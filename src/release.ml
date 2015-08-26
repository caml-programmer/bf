open Types
open Printf

exception Bad_format of string

let read file =
  let rex = Str.regexp "\\ +" in
  List.map (fun x ->
    try
      match Str.split rex x with
	| [ver;rev] -> (ver,int_of_string rev)
	| _ -> raise (Bad_format x)
    with _  -> raise (Bad_format x))
    (System.list_of_channel (open_in file))

let write file data =
  System.safewrite file
    (fun ch ->
      List.iter 
      (fun (ver,rev) ->
	output_string ch
	(sprintf "%s %d\n" ver rev)) data)
    
let update file ver rev =
  let new_rels =
    List.rev
      (List.fold_left
	(fun acc (ver',rev') ->
	  if ver = ver' && rev > rev' then
	    (ver',rev')::acc
	  else 
	    (ver,rev)::acc) []
	(read file)) in
  write file new_rels

let reg_pkg_release specdir ver rev =
  let name = "release" in
  Component.with_component_dir ~strict:true (Component.make ~label:(Branch "master") (Params.get_param "pack"))
    (fun () ->
      let file = 
	sprintf "%s/%s/%s" (Specdir.pkgname specdir) (Specdir.branch specdir) name in
      System.append_write ~file (sprintf "%s %d\n" ver rev);
      update file ver rev;
      Git.git_add file;
      Git.git_commit ~empty:true
	(sprintf "reg pkg release %s %s %s %d" 
	  (Specdir.pkgname specdir) (Specdir.branch specdir) ver rev);
      Git.git_push_cycle ~tags:false ~refspec:None "origin" 5)

(* Read release *)
   
exception Release_not_found of string
exception Bad_release_file of string

let vr_compare a b =
  let r = compare (fst b) (fst a) in
  if r = 0 then
    compare (snd b) (snd a)
  else r

let max_vr l =
  try
    Some (List.hd (List.sort vr_compare l))
  with _ -> None

let get ?(next=false) ?version specdir =
  let with_next n = if next then succ n else n in
  let make s =
    let (ver,rev) =
      let pos = String.index s ' ' in
      String.sub s 0 pos,
      with_next
	(int_of_string
	  (String.sub s (succ pos) (String.length s - pos - 1)))
    in (ver,rev) in
  let filter (v,r) =
    match version with
      | Some v' -> v' = v
      | None -> true in
  let file = Filename.concat specdir "release" in
  if Sys.file_exists file then
    begin
      try
	let ch = open_in file in
	let versions =
	  List.map make
	    (System.list_of_channel ch) in
	match max_vr (List.filter filter versions) with
	  | Some vr -> vr
	  | None ->
	      (match version with
		| Some v' ->
		    raise (Release_not_found (sprintf "%s -> no revision for version %s" file v'))
		| None ->
		    raise (Release_not_found (sprintf "%s -> no revisions" file)))
      with 
	| Release_not_found _ as e -> raise e
	| e ->
	    raise
	      (Bad_release_file 
		(sprintf "%s (%s)" file
		  (Printexc.to_string e)))
    end
  else
    raise (Release_not_found (sprintf "release file (%s) not found" file))






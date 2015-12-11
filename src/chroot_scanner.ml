(* Я написал этот модуль не просыпаясь! Я чувствую, что он говно. Перепишите, а? *)

open Ext
open Unix.LargeFile

type path = string
type file_type = File | Dir
type time = float
type file_state = file_type * time

let tts = function File -> "file" | Dir -> "dir"
let mtts = string_of_float
				
let get_ftype fstate = fst fstate
let get_mtime fstate = snd fstate

type fs_state = (path, file_state) Hashtbl.t

let create_state () = Hashtbl.create 0
let reg_fstate state path fstate = Hashtbl.add state path fstate
let files state = Hashtbl.keys state
let get_fstate state file = Hashtbl.find state file

let has_file state file =
  try Hashtbl.find state file; true
  with Not_found -> false

let rec mtime file =
  (lstat file).st_mtime

(*let mtime file = (stat file).st_mtime*)
				 
let scan dir =
  let state = create_state () in
  let reg = reg_fstate state in
  let rec scan dir =
    let files = List.map (Filename.concat dir) (System.list_of_directory dir) in
    List.iter (fun file ->
	       let mtime = mtime file in
	       if System.is_directory file then
		 (reg file (Dir,mtime); scan file)
	       else
		 (reg file (File,mtime)))
	      files in
  scan dir; state

let changes (st1:fs_state) (st2:fs_state) =
  let state = create_state () in
  let reg file fstate = (*print_endline ("REG: "^file );*) reg_fstate state file fstate in
  List.iter (fun file ->
	     let fst2 = get_fstate st2 file in
	     if has_file st1 file then
	       let fst1 = get_fstate st1 file in
	       (*
	       let t_fst1 = get_ftype fst1 in
	       let t_fst2 = get_ftype fst2 in
	       let mt_fst1 = get_mtime fst1 in
	       let mt_fst2 = get_mtime fst2 in
		*)
	       if not (((get_ftype fst1) = (get_ftype fst2))) then
		 ((*print_endline (file^" :: type "^(tts t_fst1)^" -> "^(tts t_fst2));*)
		   (* а может тут error кидать, а? это ж хрень какая-то *)
		  reg file (get_fstate st2 file))
	       else
		 if not ((get_mtime fst1) = (get_mtime fst2)) then
		   ((*print_endline (file^" :: time "^(mtts mt_fst1)^" -> "^(mtts mt_fst2));*)
		    reg file (get_fstate st2 file))
		 else ()
	     else
	       ((*print_endline (file^" :: new file");*)
		reg file (get_fstate st2 file)))
	    (files st2);
  state
    
let gen_bflist_content (st:fs_state) =
  Output.string_of_string_list
    (List.map (fun file ->
	       let (ftype, _) = get_fstate st file in
	       match ftype with
	       | File -> "f "^file
	       | Dir -> "d "^file)
	      (files st))

let filter predicate st =
  Hashtbl.filter_keys st predicate

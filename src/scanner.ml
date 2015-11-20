open Printf

type fs_entry =
  | File of string
  | Dir of string

type fs_state = (fs_entry,float) Hashtbl.t

let rec scan_entry f dir =
  let dh = Unix.opendir dir in
  let rec read acc =
    try
      read ((Unix.readdir dh)::acc)
    with End_of_file ->
      Unix.closedir dh;
      acc      
  in 
  List.iter 
    (fun s ->
      if s <> "." && s <> ".." then
	let abs = Filename.concat dir s in
	let st = Unix.LargeFile.stat abs in
	let last =
	  match st.Unix.LargeFile.st_kind with
	    | Unix.S_LNK ->
		(Unix.LargeFile.lstat abs).Unix.LargeFile.st_mtime
	    | _ -> st.Unix.LargeFile.st_mtime
	in
	if System.is_directory abs then
	  (f (Dir abs,last); scan_entry f abs)
	else
	  f (File abs,last))
    (read [])

let create_top_state dir =
  match Params.get_param "autopkg" with
    | "true" -> 
	let t = Hashtbl.create 32 in
	scan_entry 
	  (fun (entry,last) ->
	    Hashtbl.add t entry last)
	  dir;
	t
    | _ -> (Hashtbl.create 0 : fs_state)   

let strip_destdir s =
  let dest_dir = Params.get_param "dest-dir" in
  if dest_dir <> "" then
    let len = String.length dest_dir in
    if String.sub s 0 len = dest_dir then
      String.sub s len (String.length s - len)
    else s
  else s
   
let generate_changes ?(devlist=false) rules top_dir a b =
  let string_of_fs_entry = function
    | File s -> "f " ^ (strip_destdir s)
    | Dir s  -> "d " ^ (strip_destdir s)
  in
  let entry_compare a b =
    match a, b with
      | File x, File y -> compare x y
      | Dir  x, Dir  y -> compare x y
      | File x, Dir  y -> compare x y
      | Dir  x, File y -> compare x y
  in
  match Params.get_param "autopkg" with
    | "true" ->
	let acc = ref [] in
	let out s = 
	  acc := s::!acc in
	Hashtbl.iter
	  (fun b_entry b_last ->
	    (try
	      let a_last = Hashtbl.find a b_entry in
	      if b_last > a_last then
		out b_entry
	    with Not_found ->
	      out b_entry))
	  b;
	let ch = open_out
		   (match (rules,devlist) with
		    | (None, false) -> ".bf-list"
		    | (Some alt, false) -> ".bf-list" ^ alt
		    | (None, true) -> ".bf-devlist"
		    | (Some alt, true) -> ".bf-devlist" ^ alt)
	in
	output_string ch
	  (sprintf "d %s\n" top_dir);
	List.iter
	  (fun e ->
	    output_string ch (string_of_fs_entry e);
	    output_string ch "\n")
	  (List.sort entry_compare !acc);
	close_out ch
    | _ -> ()

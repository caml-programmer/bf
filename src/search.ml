(* Search *)

open Printf
open Component
open Deptree

exception Bad_component of string
exception Bad_tag_file of string

let split del s =
  let pos = String.index s del in
  let len = String.length s in
  String.sub s 0 pos,
  String.sub s (succ pos) (len - pos - 1)

let read_tag_file file =
  match System.list_of_channel (open_in file) with
    | [x] -> x
    | _   -> raise (Bad_tag_file file)

let load_tags confdir =
  printf "load tags from %s\n%!" confdir;
  let start =
    Filename.concat confdir "refs/tags" in
  let rec fs_load f tag path =
    if System.is_directory path then
      List.iter
	(fun s ->
	  let new_tag =
	    if tag = "" then s else tag ^ "/" ^ s in
	  let new_path =
	    path ^ "/" ^ s in
	  fs_load f new_tag new_path)
	(System.list_of_directory path)
    else
      f tag (read_tag_file path)
  in
  let packed_load f =
    let re = Str.regexp_string "refs/tags/" in
    let file =
      Filename.concat confdir "packed-refs" in
    if Sys.file_exists file then
      List.iter
	(fun s -> 
	  let l = String.length s in
	  if l > 0 && s.[0] <> '#' && s.[0] <> '^' then
	    begin
	      let (id,r) = split ' ' s in
	      let rl = String.length r in
	      if Str.string_match re r 0 then
		begin
		  let tag =
		    String.sub r 10 (rl - 10) in
		  f tag id
		end
	    end)
	(System.list_of_channel (open_in file))
  in
  let t = Hashtbl.create 32 in
  let f tag id =
    Hashtbl.add t tag id in

  fs_load f "" start;
  packed_load f;
  t

exception Bad_tag of string

let split_tag s =
  let (n,v) = split '/' s in
  let (v,r) = split '-' v in
  try
    (n,(v,int_of_string r))
  with _ -> raise (Bad_tag s)

let replace_value k v =
  List.map
    (fun x' -> if (fst x') = k then k,v else x')

let only_minimal_revisions tags =
  let l = ref [] in
  List.iter
    (fun (n,(v,r)) ->
      try
	let cr = List.assoc (n,v) !l in
	if r < cr then
	  l := replace_value (n,v) r !l
	else ()
      with Not_found ->
	l := ((n,v),r)::!l)
    (List.map split_tag tags);
  List.sort 
    (fun a b -> compare (fst a) (fst b)) !l  

let load_commit_tags branch commit_id =
  let make_tag_list s =
    let tag_re = Str.regexp "tag: \\([^,)]+\\)[,)]" in
    let len = String.length s in
    let rec make acc start =
      if start >= len then
	acc
      else
	(try
	  ignore(Str.search_forward tag_re s start);
	  make
	    ((Str.matched_group 1 s)::acc) (Str.match_end ())
	with Not_found -> acc)
    in make [] 0
  in
  let cmd =
    sprintf "git log --oneline --decorate --abbrev=40 %s" branch in
  let direct = ref [] in
  let reverse = ref [] in
  let add (id,tags) =
    direct := (id,tags)::!direct;
    List.iter 
      (fun tag ->
	reverse := (tag,id)::!reverse) tags
  in
  let ch = Unix.open_process_in cmd in
  try
    while true do
      let (id,rest) = split ' ' (input_line ch) in
      let tags' =
	make_tag_list rest in
      if id = commit_id then
	begin
	  add (id,tags');
	  raise Exit
	end
      else
	add (id,tags')
    done; ([],[])
  with 
    | Exit -> close_in ch; (!direct,!reverse)
    | End_of_file ->
	close_in ch; ([],[])

exception Not_found_pack
exception Bad_pack_state of string

let search_pack () =
  let parent = Filename.dirname (Sys.getcwd ()) in
  let f = Filename.concat parent in
  let a = Params.get_param "pack" in
  let b = a ^ ".git" in
  if Sys.file_exists (f a) then
    f a
  else if Sys.file_exists (f b) then
    f b
  else raise Not_found_pack

let commit_id commit_id =
  let dir = Sys.getcwd () in
  let component = Filename.basename dir in
  let confdir = ".git" in
  let packdir = search_pack () in
  
  (* Check pack state *)
  System.with_dir packdir (fun () ->
    printf "check pack state...";
    match Git.git_current_branch () with
      | Some "master" -> printf "ok\n%!"
      | _ -> raise (Bad_pack_state packdir));  

  let tag_list = load_tags confdir in
  let pack_tag_list =
    load_tags (Filename.concat packdir ".git") in
  let search_top_packages branch =
    (* Top-ы это те пакеты, которые не встречаются ни у
       какого-либо другого пакета в зависимостях *)
    let plist =
      List.fold_left
	(fun acc pkg ->
	  let specdir = Filename.concat pkg branch in
	  if Sys.file_exists specdir then
	    let depends =
	      Depends.parse (Filename.concat specdir "depends") in
	    (pkg,depends)::acc
	  else acc) []
	(List.filter System.is_directory (System.list_of_directory "."))
    in
    let mem pkg depends =
      List.exists (fun (os,deplist) -> List.exists (fun (pkg',_,_) -> pkg = pkg') deplist) depends in
    List.fold_left
      (fun acc (pkg,_) ->
	if List.exists (fun (_,depends) -> mem pkg depends) plist then
	  acc
	else pkg::acc) [] plist
  in
  let search_top_tags search_eq_tags component_branch tag =
    let packdir = search_pack () in
    let (component_package,_) = split_tag tag in
    let equivalents = search_eq_tags tag in
    let tops = ref [] in
    System.with_dir packdir
      (fun () ->
	(* Состояние pack-а необходимо перевести на момент создания тега *)
	Git.git_checkout ~low:true ~key:tag ();
	try
	  List.iter 
	    (fun pkg_branch ->	  
	      let pkg_list =
		search_top_packages pkg_branch in
	      let tree_list =
		List.fold_left
		  (fun acc pkg -> 
		    let specdir = Filename.concat pkg pkg_branch in
		    if Sys.file_exists specdir then
		      (pkg,(Top.tree_of_specdir ~log:false specdir))::acc
		    else acc) [] pkg_list in
	      let top_pkg_list_with_component =
		List.fold_left 
		  (fun acc (pkg,tree) ->
		    (* printf "top-pkg %s\n" pkg; *)
		    if
		      List.exists
			(fun (pkg',ver',rev') ->
			  let composite = sprintf "%s/composite" pkg' in (* here pkg with branch *)
			  (* printf "check %s\n" composite; *)
			  if Sys.file_exists composite then
			    List.exists
			      (fun c ->
				(c.name = component || c.name ^ ".git" = component) && c.label = Branch component_branch)
			      (Composite.components composite)
			  else false)
			(list_of_deptree tree)
		    then
		      pkg::acc
		    else acc) [] tree_list in
	      List.iter
		(fun top_pkg ->
		  let specdir = Filename.concat top_pkg pkg_branch in
		  let mktag (v,r) =
		    sprintf "%s/%s-%d" top_pkg v r in
		  if Sys.file_exists specdir then
		    begin
		      (*printf "checkout pack to state: %s for read %s/release\n%!" tag specdir;*)
		      Git.git_checkout ~low:true ~key:tag ();
		      let (v,r) = Release.get ~next:false specdir in
		      (*printf "read release %s-%d from specdir (%s)\n%!" v r specdir;*)
		      let rec find (v',r') =
			let key = mktag (v',r') in
			let check_rev () =
			  if r' > r + 2 then
			    raise Not_found
			  else
			    find (v',succ r')
			in
			if Hashtbl.mem pack_tag_list key then
			  (try
			    let tree =
			      Clonetree.tree_of_specdir ~log:false ~vr:(Some (v',r')) ~packdir specdir in
			    if
			      List.exists 
				(fun (p',v',r',_) ->
				  let p'' =
				    Filename.basename (Filename.dirname p') in
				  List.exists 
				    (fun e ->
				      let (pe,(ve,re)) = split_tag e in
				      p'' = pe && v' = ve &&  r' = re)
				    equivalents)
				(list_of_deptree tree)
			    then key
			    else
			      check_rev ()
			  with 
			    | Logger.Error -> check_rev ()
			    | Tree_error msg ->
				print_string " ## ";
				print_string msg;
				check_rev ())
			else
			  begin
			    (* printf "tag not found: %s\n%!" key; *)
			    check_rev ()
			  end
		      in 
		      try
			tops := (find (v,r))::!tops
		      with Not_found ->
			tops := (top_pkg ^ " not found by 3 up-levels"):: !tops
		    end)
		top_pkg_list_with_component)
	    (System.list_of_directory component_package);
	  
	  (* Восстанавливаем состояние pack-а *)
	  Git.git_checkout ~low:true ~key:"master" ()
	with exn ->
	  Git.git_checkout ~low:true ~key:"master" ();
	  raise exn);
    !tops
  in

  List.iter
    (fun component_branch ->
      printf "Search by component branch: %s\n%!" component_branch;
      let (direct,reverse) =
	load_commit_tags component_branch commit_id in
      let tags =
	List.map
	  (fun ((n,v),r) -> sprintf "%s/%s-%d" n v r)
	  (only_minimal_revisions
	    (List.flatten (List.map snd direct))) in
      let search_equivalent_tags tag =
	try
	  let id =
	    List.assoc tag reverse in
	  List.assoc id direct
	with Not_found -> [tag]
      in
      List.iter
	(fun tag ->
	  let tops = 
	    (try
	      search_top_tags search_equivalent_tags component_branch tag
	    with Not_found -> [])
	  in
	  printf "> %s %s:\n%!" (Hashtbl.find tag_list tag) tag;
	  List.iter (printf " >> %s\n%!") tops) tags)
    (Git.git_branch ())


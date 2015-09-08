(* Clean *)

open Printf

let pkg_compare a b =
  let (dir_a,name_a,pkg_a,platform_a,ext_a,arch_a,ver_a,rev_a) = snd a in
  let (dir_b,name_b,pkg_b,platform_b,ext_b,arch_b,ver_b,rev_b) = snd b in
  match compare name_a name_b with
    | 0 ->
	(match Version.compare (Version.parse ver_a) (Version.parse ver_b) with
	  | 0 -> compare rev_a rev_b
	  | x -> x)
    | x -> x
  
let packages () =
  let t = Hashtbl.create 32 in
  let pkgs =
    List.sort pkg_compare
      (List.fold_left
	(fun acc s ->
	  try
	    (s,Pkgpath.parse s)::acc
	  with _ -> acc)
	[] (System.list_of_directory ".")) in
  List.iter
    (fun (s,x) ->
      let (_,name,_,_,_,_,ver,rev) = x in
      if Hashtbl.mem t name then
	Hashtbl.replace t name ((s,ver,rev)::(Hashtbl.find t name))
      else
	Hashtbl.add t name [s,ver,rev])
    pkgs;
  let droplist = ref [] in
  let drop s =
    droplist := s::!droplist in
  Hashtbl.iter
    (fun name list ->
      let first = ref true in
      printf "%s: %s\n" name
	(String.concat " "
	  (List.map
	    (fun (s,ver,rev) ->
	      let label =
		ver ^ "-" ^ string_of_int rev in
	      if !first then
		begin first := false; "[" ^ label ^ "]" end
	      else
		begin drop s; label end) list))) t;
  let tops =
    List.filter
      (fun x -> not (List.mem (fst x) !droplist)) pkgs in
  let drop_list_without_deps =
    List.fold_left
      (fun acc (s,x) ->
	let (_,name,_,_,_,_,ver,rev) = x in
	let deps = Pkgdeps.extract ~userhost:None s in
	List.fold_left
	  (fun acc (pkg_name,ver_opt,rev_opt,_) ->
	    match ver_opt, rev_opt with
	      | Some ver, Some rev ->
		  List.filter 
		    (fun drop ->
		      let pat =
			sprintf "%s/%s/%d" pkg_name ver rev in
		      let result = not (Strings.substring_exists pat drop) in
		      if not result then
			begin
			  printf "remove %s from droplist by depends from %s\n%!" drop s
			end;
		      result) acc
	      | _ -> acc) acc deps) !droplist tops in
  try
    while true do
      printf "Clean unselected revisions? (y/n/p): %!";
      match input_line stdin with
	| "y" ->
	    List.iter Unix.unlink drop_list_without_deps;
	    raise Exit
	| "n" ->
	    raise Exit
	| "p" ->
	    List.iter print_endline drop_list_without_deps;
	    raise Exit
	| _ -> ()
    done
  with Exit -> ()

let droptags lifetime =
  let period =
    Lifetime.parse lifetime in
  Lifetime.iter period Git.resolve_tag_date
    (fun tag ->
      printf "drop %s\n%!" tag;
      Git.git_drop_tag tag)
    (Git.git_tag_list ())

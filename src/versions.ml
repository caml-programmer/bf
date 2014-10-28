(* Versions *)

open Printf

exception Bad_pkgdir of string
exception Bad_release of string

let last pkgdir =
  let pkgname =
    Filename.basename pkgdir in
  let slash_re = Str.regexp_string "/" in
  let minus_re = Str.regexp_string "-" in
  
  let packdir = 
    Filename.dirname pkgdir in

  let read_pkg_version b = 
    let release =
      Filename.concat b "release" in
    if Sys.file_exists release then
      match Str.split (Str.regexp "\\ +") (Lists.last (System.list_of_channel (open_in release))) with
	| [ver;_] -> ver
	| _ ->
	    raise (Bad_release release)
    else b
  in

  if 
    Filename.basename packdir <> "pack" &&
    Filename.basename packdir <> "pack.git" 
  then
    raise (Bad_pkgdir pkgdir);

  System.with_dir packdir (fun () -> Git.git_pull "origin");
  
  System.with_dir pkgdir
    (fun () ->
      let branches =
	List.map
	  read_pkg_version (System.list_of_directory ".") in
      let t = ref ([] : ((string * string) * int) list) in
      let update (k,v) =
	if List.mem_assoc k !t then
	  let v' =  List.assoc k !t in
	  if v > v' then
	    t := List.map (fun (k',v') -> if k = k' then (k',v) else (k',v')) !t
	  else ()
	else	  
	  t := (k,v)::!t
      in
      let compare a b =
	compare (snd (fst a)) (snd (fst b)) in
      List.iter	update
	(List.fold_left
	  (fun acc tag ->
	    match Str.split slash_re tag with
	      | [pkg; fullver] ->
		  (try
		    let l =
		      Str.split minus_re fullver in
		    let ver = List.hd l in
		    let rev = int_of_string (List.hd (List.tl l)) in
		    if pkg = pkgname then
		      if List.mem ver branches then
			((pkg,ver),rev)::acc
		      else acc
		    else acc
		  with _ -> acc)
	      | _ -> acc)
	  [] (Git.git_tag_list ()));
      List.iter 
	(fun ((p,v),r) ->
	  printf "- %s/%s-%d\n%!" p v r) (List.sort compare !t))

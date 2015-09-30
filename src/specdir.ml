open Printf
open Output
       
let pkgname specdir =
  Filename.basename (Filename.dirname specdir)

let branch s =
  Filename.basename s   

let get_version file =
  let s = System.read_file file in
  try
    String.sub s 0 (String.index s '\n')
  with Not_found -> s

let pack_branches pkgdir =
  List.filter (fun s -> s <> "hooks.scm")
    (System.list_of_directory pkgdir)

exception Pack_branch_is_not_found of string

let select_branch ~default_branch pkgdir pkg =
  match pack_branches (Filename.concat pkgdir pkg) with
    | [] -> raise (Pack_branch_is_not_found pkg)
    | hd::tl as branches ->
	(match tl with
	  | [] -> hd
	  | _ ->
	      begin
		let select () =
		  printf "Select pack-branch for %s\n%!" pkg;
		  let pack_branch_variants =
		    Array.of_list branches in
		  Array.iteri (printf "%d) %s\n%!") pack_branch_variants;
		  let n = Interactive.read_number (pred (List.length branches)) in
		  pack_branch_variants.(n)
		in
		match default_branch with
		  | Some b ->
		      if List.mem b branches then b
		      else raise
			(Pack_branch_is_not_found pkg)
		  | None -> select ()
	      end)

let of_pkg ~default_branch pkgdir pkg =
  sprintf "%s/%s/%s" pkgdir pkg (select_branch ~default_branch pkgdir pkg)

let last_line_of_file filename =
  let channel = open_in filename in
  let line = ref "" in
  let rec read () =
    try line := input_line channel;
	read ()
    with End_of_file -> close_in channel;
			!line in
  read ()

let ver_rev_of_release release =
  let err msg = err "ver_rev_of_release" msg in
  match Str.split (Str.regexp " ") release with
  | [ver;rev] -> ver,rev
  | x -> err ("Invalid release: \""^(String.concat "" x)^"\"")

exception No_release_file
	     
let release_by_specdir specdir =
  let err msg = err "release_by_specdir" msg in
  let release_file = Path.make [specdir;"release"] in
  if Sys.file_exists release_file then
    last_line_of_file release_file
  else raise No_release_file

let specdir_by_version pkg_name version =
  let err msg = err "specdir_by_version" msg in
  let warn msg = warn "specdir_by_version" msg in
  let pkg_dir = Path.make ["pack";pkg_name] in
  let specdirs = List.map (fun dir -> Path.make [pkg_dir;dir])
			  (System.list_of_directory pkg_dir) in
  let rec check_specdir_version specdirs =
    match specdirs with
    | specdir :: specdirs ->
       (try
	   let (ver,rev) = ver_rev_of_release (release_by_specdir specdir) in
	   if ver = version then specdir else check_specdir_version specdirs
	 with No_release_file -> warn ("No release file in directory '"^specdir^"'");
				 check_specdir_version specdirs)
    | [] -> err ("specdir for version "^version^" not found") in
  check_specdir_version specdirs

  

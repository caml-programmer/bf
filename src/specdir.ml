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

exception Invalid_release of string
       
let ver_rev_of_release release =
  let err msg = err "ver_rev_of_release" msg in
  match Str.split (Str.regexp " ") release with
  | [ver;rev] -> ver,(int_of_string rev)
  | x -> raise (Invalid_release (String.concat " " x))

exception No_release_file
	     
let release_by_specdir specdir =
  let err msg = err "release_by_specdir" msg in
  let release_file = Path.make [specdir;"release"] in
  if Sys.file_exists release_file then
    last_line_of_file release_file
  else raise No_release_file

let ver_rev_by_specdir specdir =
  ver_rev_of_release (release_by_specdir specdir)

let specdir_by_version pkg_name version =
  let err msg = err "specdir_by_version" msg in
  let warn msg = warn "specdir_by_version" msg in
  let pack_param = Params.get "pack" in
  let pkg_dir = Path.make [pack_param;pkg_name] in
  let specdirs = List.map (fun dir -> Path.make [pkg_dir;dir])
			  (System.list_of_directory pkg_dir) in
  let rec check_specdir_version specdirs =
    match specdirs with
    | specdir :: specdirs ->
       (try
	   let (ver,rev) = ver_rev_of_release (release_by_specdir specdir) in
	   if ver = version then specdir else check_specdir_version specdirs
	 with
	 | No_release_file -> warn ("No release file in directory '"^specdir^"'");
			      check_specdir_version specdirs
	 | Invalid_release release -> warn ("Invalid release in specdir '"^specdir^"': "^release);
				      check_specdir_version specdirs)
    | [] -> err ("specdir for pkgver '"^pkg_name^" "^version^"' not found") in
  check_specdir_version specdirs

  
let revision_by_pkgver pkgname version =
  let (_, rev) = ver_rev_by_specdir (specdir_by_version pkgname version) in rev

let pkg_versions pkg_name =
  let pack_param = Params.get "pack" in
  let pkg_path = Path.make [pack_param; pkg_name] in
  let specdirs = List.map (fun specdir -> Path.make [pkg_path; specdir])
			  (System.list_of_directory pkg_path) in
  List.map (fun specdir ->
	    let (ver,_) = ver_rev_of_release (release_by_specdir specdir) in ver)
	   specdirs
									      
(* Хак для обработки >= *)
let find_max_version pkg_name =
  let pack_param = Params.get "pack" in
  let pkg_dir = Path.make [pack_param;pkg_name] in
  let specdirs = List.map (fun dir -> Path.make [pkg_dir;dir])
			  (System.list_of_directory pkg_dir) in
  let rec search maxver = function
    | specdir :: specdirs ->
       (try let (ver,_) = ver_rev_of_release (release_by_specdir specdir) in
	    let maxver = Version.max maxver ver in
	    search maxver specdirs
	with No_release_file | Invalid_release _ -> search maxver specdirs)
    | [] -> maxver in
  search "" specdirs

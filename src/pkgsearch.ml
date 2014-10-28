(* Pkg search *)

open Platform
open Logger
open Printf

exception Broken_pkg_iteration of string
exception Cannot_find_pkgver of string
exception Cannot_find_pkgrev of string

let tag_extraction_rex pkgname = function
  | Debian ->
      Pcre.regexp
	(pkgname ^ "-([^-]+)-(\\d+)\\." ^ (Debian.fix_arch (System.arch ())) ^ "\\.deb")
  | platform ->
      Pcre.regexp
	(pkgname ^ "-([^-]+)-(\\d+)\\." ^ (string_of_platform platform) ^ "\\." ^ (System.arch ()) ^ "\\.")

let map_pkg f pkgname =
  try
    with_platform
      (fun os platform ->
	let pkgname =
	  match engine_of_platform platform with
	    | Rpm_build 
	    | Deb_pkg   -> pkgname
	    | Pkg_trans -> Pkgtrans.name_format pkgname
	in
	let rex = tag_extraction_rex pkgname platform in
	let ff acc s =
	  if Pcre.pmatch ~rex s then
	    let a = Pcre.extract ~rex s in
	    if Array.length a > 2 then
	      (a.(1),int_of_string a.(2))::acc
	    else acc
	  else acc
	in
	List.map f
	  (List.fold_left ff []
	    (System.list_of_directory (Sys.getcwd ()))))
  with exn ->
    raise (Broken_pkg_iteration (Printexc.to_string exn))

let filter_pkg f pkgname =
  List.filter f
    (map_pkg (fun x -> x) pkgname)

let version ?(interactive=false) pkgname =
  try
    (match
      List.sort 
	(fun a b -> compare b a)
	(map_pkg fst pkgname)
    with [] -> raise Not_found
      | hd::tl -> hd)
  with exn ->
    if interactive then
      begin
	log_message (sprintf "Enter package version for %s" pkgname);
	Interactive.read_string ()
      end
    else
      raise (Cannot_find_pkgver (Printexc.to_string exn))

let revision ?(interactive=false) pkgname version =
  try
    (match
      List.sort
	(fun a b -> compare b a)
	(List.map snd
	  (filter_pkg
	    (fun (ver,_) -> ver = version) pkgname))
    with [] -> raise Not_found
      | hd::tl -> hd)
  with exn ->
    if interactive then
      begin
	log_message (sprintf "Enter package revision for %s-%s" pkgname version);
	Interactive.read_number 0	
      end
    else
      raise (Cannot_find_pkgrev (Printexc.to_string exn))

open Printf
open Platform

type pack_branch = string

type t = {
  pkg_path : string;
  pkg_dir : string;
  pkg_name : string;
  pkg_fullname : string;
  pkg_platform : platform;
  pkg_extension : string;
  pkg_arch : string;
  pkg_version : Component.version;
  pkg_revision : Component.revision;
  pkg_branch: pack_branch;
}

exception Cannot_extract_arch of string
exception Cannot_extract_platform of string
exception Cannot_resolve_dependes of string
exception Cannot_resolve_soft_dependes of string
exception Cannot_extract_revision of string
exception Cannot_extract_version of string
exception Cannot_extract_extension of string
exception Cannot_extract_pkgname of string

let extract_extension pkg_name =
  try
    let pos = String.rindex pkg_name '.' in
    String.sub pkg_name (succ pos) (String.length pkg_name - pos - 1)
  with _ ->
    raise (Cannot_extract_extension pkg_name)

let extract_arch pkg_name =
  try
    let pos1 = String.rindex pkg_name '.' in
    let pos2 = String.rindex_from pkg_name (pred pos1) '.' in
    String.sub pkg_name (succ pos2) (pos1 - pos2 - 1)
  with _ ->
    raise (Cannot_extract_arch pkg_name)

let extract_platform pkg_name =
  try
    let pos0 = String.rindex pkg_name '.' in
    let pos1 = String.rindex_from pkg_name (pred pos0) '.' in
    let pos2 = String.rindex_from pkg_name (pred pos1) '.' in
    platform_of_string 
      (String.sub pkg_name (succ pos2) (pos1 - pos2 - 1))
  with _ ->
    raise (Cannot_extract_platform pkg_name)

let extract_revision rest pkg_name =
  try
    let rest_len = String.length rest in
    let s = 
      String.sub pkg_name 0
	(String.length pkg_name - rest_len) in
    let pos = String.rindex s '-' in
    int_of_string 
      (String.sub s (succ pos)
	(String.length s - pos - 1))
  with _ ->
    raise (Cannot_extract_revision pkg_name)

let extract_version rest pkg_name =
  try
    let rest_len = String.length rest in
    let s = 
      String.sub pkg_name 0
	(String.length pkg_name - rest_len) in
    let pos = String.rindex s '-' in
    String.sub s (succ pos)
      (String.length s - pos - 1)
  with _ ->
    raise (Cannot_extract_version pkg_name)

let extract_name rest pkg =
  try
    String.sub pkg 0 (String.length pkg - String.length rest)
  with _ -> raise (Cannot_extract_pkgname pkg)

let name pkg_path =  
  let pkg = Filename.basename pkg_path in
  let platform = extract_platform pkg in
  let extension = extract_extension pkg in
  let arch = extract_arch pkg in
  let revision = extract_revision (sprintf ".%s.%s.%s" (string_of_platform platform) arch extension) pkg in
  let version = extract_version (sprintf "-%d.%s.%s.%s" revision (string_of_platform platform) arch extension) pkg in
  extract_name (sprintf "-%s-%d.%s.%s.%s" version revision (string_of_platform platform) arch extension) pkg

let parse pkg_path =
  let pkg_dir = Filename.dirname pkg_path in
  let pkg = Filename.basename pkg_path in
  let platform = extract_platform pkg in
  let extension = extract_extension pkg in
  let arch = extract_arch pkg in
  let revision =
	extract_revision (sprintf ".%s.%s.%s" (string_of_platform platform) arch extension) pkg in
  let version = 
    extract_version (sprintf "-%d.%s.%s.%s" revision (string_of_platform platform) arch extension) pkg in
  let pkg_name =
    extract_name (sprintf "-%s-%d.%s.%s.%s" version revision (string_of_platform platform) arch extension) pkg in
  (pkg_dir,pkg_name,pkg,platform,extension,arch,version,revision)

let make_pkg_record ~userhost pkg_path =
  let (pkg_dir,pkg_name,pkg,platform,extension,arch,version,revision) = parse pkg_path in
  let engine = engine_of_platform platform in
  let pack_branch =
    Pkgbuild.extract_packbranch ?userhost ~engine pkg_path in
  { 
    pkg_path = pkg_path;
    pkg_dir = pkg_dir;
    pkg_name = pkg_name;
    pkg_fullname = pkg;
    pkg_platform = platform;
    pkg_extension = extension;
    pkg_arch = arch;
    pkg_version = version;
    pkg_revision = revision;
    pkg_branch = pack_branch;
  }

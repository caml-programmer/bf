open Platform
open Printf
open Spectype

exception Invalid_specdir_format
exception Unsupported_specdir_version of string

let v1 specdir =
  let flist = ["rh.spec";"rh.files";"rh.req"] in
  if System.is_directory specdir &&
    List.for_all
    (fun s ->
      Sys.file_exists (Filename.concat specdir s))
    flist
  then
    List.map (Filename.concat specdir) flist
  else raise Invalid_specdir_format

let v2 ?(snapshot=false) ?(short_composite=false) ~version ~revision specdir =
  let f = Filename.concat specdir in
  let pkgname = 
    Filename.basename (Filename.dirname specdir) in
  let pack_branch = Filename.basename specdir in
  let load s =
    if Sys.file_exists s then
      let ch = open_in s in
      let content =
	System.string_of_channel ch in
      close_in ch;
      Some content
    else
      None
  in
  let depends = 
    if snapshot then
      Depends.load ~snapshot:(version,revision) (f "depends")
    else
      Depends.load (f "depends") in
  let rejects =
    let n = f "rejects" in
    if Sys.file_exists n then
      System.list_of_channel (open_in n)
    else []
  in
  let provides =
    let n = f "provides" in
    let p =
      with_platform
	(fun os platform ->
	  sprintf "packbranch-%s = %s-%s-%s.%s" pack_branch pkgname
	  version revision (string_of_platform platform)) in
    if Sys.file_exists n then
      p::(System.list_of_channel (open_in n))
    else [p]
  in
  let obsoletes =
    let n = f "obsoletes" in
    if Sys.file_exists n then
      System.list_of_channel (open_in n)
    else []
  in
  let components =
    Composite.components ~short_composite (f "composite") in
  let pre_install =
    load (f "pre-install") in
  let pre_update =
    load (f "pre-update") in
  let pre_uninstall =
    load (f "pre-uninstall") in
  let post_install =
    load (f "post-install") in
  let params =
    let n = f "params" in
    if Sys.file_exists n then
      Params.read_from_file n
    else Hashtbl.create 0
  in
  let hooks =
    let n = f "hooks.scm" in
    if Sys.file_exists n then
      Some n
    else
      let parent = Filename.dirname specdir in
      let pn = Filename.concat parent "hooks.scm" in
      if Sys.file_exists pn then
	Some pn
      else None
  in
 
  {
    pkgname = pkgname;
    depends = depends;
    provides = provides;
    obsoletes = obsoletes;
    rejects = rejects;
    components = components;
    pre_install = pre_install;
    pre_update = pre_update;
    pre_uninstall = pre_uninstall;
    post_install = post_install;
    params = params;
    hooks = hooks;
  }

let v3 ?(snapshot=false) ~version ~revision specdir =
  v2 ~short_composite:true ~snapshot ~version ~revision specdir

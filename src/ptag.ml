(* Prefixed Tag *)

open Spectype
open System
       
let dafault_prefix () = Params.get "tag-prefix"

let make_template ?(prefix=dafault_prefix()) pkg ver =
  prefix^pkg^"/"^ver

let make ?(prefix=dafault_prefix()) pkg ver rev =
  prefix^pkg^"/"^ver^"-"^(string_of_int rev)

let by_spec ?(prefix=dafault_prefix()) spec =
  make ~prefix spec.pkgname spec.version spec.revision
  
let find_max_rev ?(prefix=dafault_prefix()) pkg ver =
  let template = prefix^pkg^"/"^ver in
  let packdir = Params.get "pack" in
  try
    let tag = with_dir packdir (fun () -> Git.git_tag_lookup_last template) in
    let st_pos = succ (String.length template) in
    let len = (String.length tag) - st_pos in
    int_of_string (String.sub tag st_pos len)
  with Not_found -> -1 (* если не было вообще, надо -1 вернуть, тогда запакетится 0я версия *)


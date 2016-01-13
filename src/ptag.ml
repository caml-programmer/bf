(* Prefixed Tag *)

open Spectype

let get_prefix () = Params.get "tag-prefix"

let make ?(prefix=get_prefix()) pkg ver rev =
  prefix^pkg^"/"^ver^"-"^(string_of_int rev)

let by_spec ?(prefix=get_prefix()) spec =
  make ~prefix spec.pkgname spec.version spec.revision
  

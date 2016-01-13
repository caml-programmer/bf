
let get_prefix () = Params.get "tag-prefix"

let tag ?(prefix=get_prefix()) pkg ver rev =
  prefix^pkg^"/"^ver^"-"^(string_of_int rev)

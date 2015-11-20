
let make dir_list =
  let double_slash_regexp = Str.regexp "//" in
  let rec remove_double_slash str =
    try ignore (Str.search_forward double_slash_regexp str 0);
	remove_double_slash (Str.global_replace double_slash_regexp "/" str)
    with Not_found -> str in
  remove_double_slash (String.concat "/" dir_list)

let is_absolute path =
  let first_symbol = String.sub path 0 1 in
  first_symbol = "/"

let make_absolute path =
  if is_absolute path then
    path
  else
    make [Sys.getcwd (); path]

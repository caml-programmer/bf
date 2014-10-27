let rec last = function
  | [] -> raise Not_found
  | hd::[] -> hd
  | hd::tl -> last tl

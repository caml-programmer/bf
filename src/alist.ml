
exception Key_already_exists
exception Key_not_found
				  
let assoc = List.assoc

let has_key = List.mem_assoc

let remove key alist =
  if has_key key alist then
    List.remove_assoc key alist
  else raise Key_not_found

let add key value alist =
  if has_key key alist then
    raise Key_already_exists
  else (key,value) :: alist

let keys alist =
  let (keys, _) = List.split alist in keys

let values alist =
  let (_, values) = List.split alist in values

let update ?(create=false) key value alist =
  if has_key key alist then
    add key value (remove key alist)
  else
    if create then
      add key value alist
    else
      raise Key_not_found
    


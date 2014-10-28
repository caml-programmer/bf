let name_format s =
  try
    let pos = String.index s '-' in
    let r = String.sub s 0 (String.length s) in
    r.[pos] <- 'D';
    for i=0 to pos do
      r.[i] <- Char.uppercase r.[i]
    done; r
  with Not_found -> s


let key_format = String.uppercase

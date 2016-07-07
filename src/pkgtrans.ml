let name_format s =
  try
    let pos = Bytes.index s '-' in
    let r = Bytes.sub s 0 (Bytes.length s) in
    Bytes.set r pos 'D';
    for i=0 to pos do
      Bytes.set r i (Char.uppercase (Bytes.get r i))
    done; r
  with Not_found -> s

let key_format = Bytes.uppercase

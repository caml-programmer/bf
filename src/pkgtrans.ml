let name_format s =
  try
    let bs = Bytes.of_string s in
    let pos = Bytes.index bs '-' in
    let r = Bytes.sub bs 0 (Bytes.length bs) in
    Bytes.set r pos 'D';
    for i=0 to pos do
      Bytes.set r i (Char.uppercase_ascii (Bytes.get r i))
    done;
    Bytes.to_string r
  with Not_found -> s

let key_format = String.uppercase_ascii

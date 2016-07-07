let split c s =
  let rec make acc ofs =
    try
      let pos = 
	Bytes.index_from s ofs c in
      (Bytes.sub s ofs (pos - ofs))::(make acc (succ pos))
    with Not_found ->
      (Bytes.sub s ofs ((Bytes.length s) - ofs))::acc
  in make [] 0

let substring_exists s' s =
  let l' = Bytes.length s' in
  let l  = Bytes.length s in  
  let rec check start =
    if start >= l then
      false
    else
      (try
	let pos = 
	  Bytes.index_from s start s'.[0] in
	if pos + l' <= l then
	  (Bytes.sub s start l' = s' || check (succ start))
	else false
      with Not_found -> 
	check (succ start))
  in
  if l >=l' && l > 0 then
    if l' = 0 then true
    else
      check 0
  else false

let substring_replace (k,v) s =
  let l' = Bytes.length k in
  let l  = Bytes.length s in
  let rec replace start =
    if start >= l then
      s
    else
      (try
	let pos = 
	  Bytes.index_from s start k.[0] in
	if pos + l' <= l then
	  begin
	    if Bytes.sub s start l' = k then
	      (Bytes.sub s 0 start) ^ v ^ (Bytes.sub s (start + l') (l - l' - start))
	    else
	      replace (succ start)
	  end
	else s
      with Not_found -> 
	replace (succ start))
  in
  if l >=l' && l > 0 then
    if l' = 0 then s
    else
      replace 0
  else s

let without_first_symbol s =
  let len = Bytes.length s in
  if len > 0 then
    Bytes.sub s 1 (len - 1)
  else s

let have_prefix prefix s =
  let len = Bytes.length s in
  let plen = Bytes.length prefix in
  len >= plen && prefix = Bytes.sub s 0 plen
  
let have_suffix suffix s =
  let len = Bytes.length s in
  let slen = Bytes.length suffix in
  len >= slen && suffix = Bytes.sub s (len - slen) slen

let drop_first_spaces s =
  let b = Buffer.create 32 in
  let state = ref 0 in
  Bytes.iter
    (function
      | ' '
      | '\t' as c ->
	  if !state = 1 then
	    Buffer.add_char b c
      | c ->
	  state := 1;
	  Buffer.add_char b c) s;
  Buffer.contents b

let drop_second_spaces s =
  let len = ref (Bytes.length s) in
  while !len > 0 && s.[pred !len] = ' ' || s.[pred !len] = '\t' do
    decr len
  done;
  Bytes.sub s 0 !len

let drop_spaces s =
  drop_second_spaces (drop_first_spaces s)

let string_of_char = Char.escaped

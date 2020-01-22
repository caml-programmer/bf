let ( ^^ ) = Bytes.cat

let split c s =  
  let rec make acc ofs =
    try
      let pos = 
	String.index_from s ofs c in
      (String.sub s ofs (pos - ofs))::(make acc (succ pos))
    with Not_found ->
      (String.sub s ofs ((String.length s) - ofs))::acc
  in make [] 0

let substring_exists s' s =
  let l' = String.length s' in
  let l  = String.length s in  
  let rec check start =
    if start >= l then
      false
    else
      (try
	let pos = 
	  String.index_from s start s'.[0] in
	if pos + l' <= l then
	  (String.sub s start l' = s' || check (succ start))
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
  let bk = Bytes.of_string k in
  let bv = Bytes.of_string v in
  let bs = Bytes.of_string s in  
  let l' = Bytes.length bk in
  let l  = Bytes.length bs in
  let rec replace start =
    if start >= l then
      bs
    else
      (try
	let pos = 
	  Bytes.index_from bs start (Bytes.get bk 0) in
	if pos + l' <= l then
	  begin
	    if Bytes.sub bs start l' = bk then
	      (Bytes.sub bs 0 start) ^^ bv ^^ (Bytes.sub bs (start + l') (l - l' - start))
	    else
	      replace (succ start)
	  end
	else bs
      with Not_found -> 
	replace (succ start))
  in
  if l >=l' && l > 0 then
    if l' = 0 then s
    else
      Bytes.to_string (replace 0)
  else s

let without_first_symbol s =
  let len = Bytes.length s in
  if len > 0 then
    Bytes.sub s 1 (len - 1)
  else s

let have_prefix prefix s =
  let len = String.length s in
  let plen = String.length prefix in
  len >= plen && prefix = String.sub s 0 plen
  
let have_suffix suffix s =
  let len = String.length s in
  let slen = String.length suffix in
  len >= slen && suffix = String.sub s (len - slen) slen

let drop_first_spaces s =
  let b = Buffer.create 32 in
  let state = ref 0 in
  String.iter
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
  let len = ref (String.length s) in
  while !len > 0 && String.get s (pred !len) = ' ' || String.get s (pred !len) = '\t' do
    decr len
  done;
  String.sub s 0 !len

let drop_spaces (s: string) : string =
  drop_second_spaces (drop_first_spaces s)
    
let string_of_char = Char.escaped

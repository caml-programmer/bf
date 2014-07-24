exception Bad_version of string

let clear s =
  let b = Buffer.create 32 in
  String.iter
    (function
      | '0' .. '9' as c -> Buffer.add_char b c
      | _ -> ()) s;
  Buffer.contents b

let parse x =
  try
    List.map 
      (fun s ->
	int_of_string (clear s))
      (List.filter ((<>) "")
	(Strings.split '.' x))
  with exn ->
    raise (Bad_version x)

let capacity x =
  List.length (parse x)

let compare a b =
  let rec cmp acc = function
    | [],_ -> acc
    | _,[] -> acc
    | hd1::tl1, hd2::tl2 ->
	let r = compare hd1 hd2 in
	if r = 0 then
	  cmp acc (tl1,tl2)
	else r in
  let init =
    compare (List.length a) (List.length b) in
  cmp init (a,b)

let exists x =
  let n = ref false in
  let s = ref false in
  let d = ref false in
  String.iter
    (function
      | '0' .. '9' -> n := true;
      | '-'        -> s := true;
      | '.'        -> d := true;
      | _ -> ()) x;
  !n && !s && !d

let is x =
  try
    List.length (parse x) > 0
  with _ -> false

let build v =
  String.concat "." (List.map string_of_int v)

exception Bad_increment_position of (string * int)

let increment position x =
  let v = parse x in
  let rec update n = function
    | [] -> []
    | hd::tl ->
	let new_hd =
	  if n = position then
	    succ hd
	  else
	    hd
	in new_hd::(update (succ n) tl) in 
  build (update 0 v)

let truncate len x =
  let v = parse x in
  let rec mk n = function
    | [] -> []
    | hd::tl ->	
	if n > 0 then
	  hd::(mk (pred n) tl)
	else []
  in build (mk len v)
	  

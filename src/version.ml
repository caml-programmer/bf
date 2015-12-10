exception Bad_version of string
exception Bad_version_for_major_increment of string

let of_release release =
  List.hd (Output.string_list_of_string ~separator:"-" release)

let chop_suffix_of_revision release =
  List.hd (Output.string_list_of_string ~separator:"\\." release)

let ver_rev_of_release release =
  let err = Output.err "Version.ver_rev_of_release" in
  match Output.string_list_of_string ~separator:"-" release with
  | [ver;rev] -> (ver,(chop_suffix_of_revision rev))
  | [ver] -> (ver,"")
  | _ -> err ("Bad release: "^release)

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

let compare ?(retype=fun _ -> None) a b =
  let rec cmp acc = function
    | [],_ -> acc
    | _,[] -> acc
    | hd1::tl1, hd2::tl2 ->
	let r =
	  match retype hd1, retype hd2 with
	    | Some hd1, Some hd2 -> compare hd1 hd2
	    | _, _               -> compare hd1 hd2 in
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
	  
let null_extend n =
  let rec mk acc = function
    | 0 -> acc
    | n -> mk ("0"::acc) (pred n)
  in String.concat "." (mk [] n)

let major_increment base ver =
  match parse ver with
    | major::_ ->
	string_of_int (succ major) ^ "." ^
	null_extend (pred base)
    | _ ->
	raise (Bad_version_for_major_increment ver)
    
let minor_increment base ver =
  increment (pred base) ver

let extend ver = ver ^ ".1"

let write file version =
  System.write (version ^ "\n") file

let have_revision vr_opt =
  match vr_opt with
    | None -> false
    | Some (ver,rev_opt) ->
	(match rev_opt with
	  | Some _ -> true
	  | None -> false)

let parse_vr_opt vr_opt =
  let make_ver v =
    try
      let pos = String.index v '-' in
      let len =
	try
	  String.index_from v pos '.'
	with Not_found -> String.length v
		      in
      String.sub v 0 pos,
      Some (int_of_string (String.sub v (succ pos) (len - pos - 1)))
    with Not_found -> v,None
  in
  (match vr_opt with
    | Some (op,ver) -> Some (make_ver ver)
    | None          -> None)

let max v1 v2 =
  let v1_list = parse v1 in
  let v2_list = parse v2 in
  match compare v1_list v2_list with
  | 1 | 0 -> v1
  | -1 -> v2
  | _ -> failwith "Will never happen"

let cmp v1 v2 =
  let v1_list = parse v1 in
  let v2_list = parse v2 in
  compare v1_list v2_list

let less v1 v2 =
  let v1_list = parse v1 in
  let v2_list = parse v2 in
  match compare v1_list v2_list with
  | -1 -> true
  | _ -> false

let greater v1 v2 =
  let v1_list = parse v1 in
  let v2_list = parse v2 in
  match compare v1_list v2_list with
  | 1 -> true
  | _ -> false

let equal v1 v2 =
  v1 = v2



let parse x =
  List.map int_of_string
    (List.filter ((<>) "")
      (Strings.split '.' x))

let capacity x =
  List.length (parse x)

let compare a b =
  let rec cmp acc = function
    | [],_ -> acc
    | _,[] -> acc
    | hd1::tl1, hd2::tl2 ->
	let r = compare hd1 hd2 in
	if r = 0 then
	  cmp r (tl1,tl2)
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

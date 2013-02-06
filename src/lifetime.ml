type lifetime =
  | LT_year of int
  | LT_mon of int
  | LT_day of int

exception Invalid_period of lifetime
exception Bad_period of string

let parse s =
  try
    let len = String.length s in
    let num =
      int_of_string (String.sub s 0 (len - 1)) in
    (match s.[len-1] with
      | 'y' -> LT_year num
      | 'm' -> LT_mon num
      | 'd' -> LT_day num
      |  _  -> raise (Bad_period s))
  with _ -> raise (Bad_period s)

let sub_interval cur period =
  let norm_p = function
    | LT_year y ->
	(y,0,0)
    | LT_mon m ->
	if m > 11 then
	  ((m / 12),m mod 12,0)
	else (0,m,0)
    | LT_day d ->
	if d > 29 then
	  let m = d / 30 in
	  let d = d mod 30 in
	  if m > 11 then
	    let y = m / 12 in
	    let m = m mod 12 in
	    (y,m,d)
	  else
	    (0,m,d)
	else
	  (0,0,d) in
  let rec norm_c x =
    let (y,m,d) = x in
    if d < 0 then
      let d = 30 + d in
      let m = pred m in
      norm_c (y,m,d)
    else
      if m < 0 then
	let m = 12 + m in
	let y = pred y in
	norm_c (y,m,d)
      else
	if y < 0 then
	  raise (Invalid_period period)
	else (y,m,d) in  
  let (cy,cm,cd) = cur in
  let (py,pm,pd) = norm_p period in
  norm_c (cy-py,cm-pm,cd-pd)

let iter period resolver call data =
  let tm = Unix.localtime (Unix.time ()) in
  let (my,mm,md) =
    sub_interval
      (tm.Unix.tm_year + 1900,
      tm.Unix.tm_mon + 1,
      tm.Unix.tm_mday) period in
  List.iter
    (fun value ->
      let (cy,cm,cd) = resolver value in
      if cy < my then
	call value
      else if cy = my then
	begin
	  if cm < mm then		  
	    call value
	  else if cm = mm then
	    begin
	      if cd < md then
		call value
	    end
	end)
    data



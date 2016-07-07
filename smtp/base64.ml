(* From ocamlnet *)

let b64_pattern plus slash =
  [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
  'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z';
  'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
  'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
  '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; plus; slash |];;


let rfc_pattern = b64_pattern '+' '/';;
let url_pattern = b64_pattern '-' '/';;

let encode_with_options b64 equal s pos len 
  linelen first_linelen crlf =
  (* encode using "base64".
   * 'b64': The encoding table, created by b64_pattern.
   * 'equal': The character that should be used instead of '=' in the original
   *          encoding scheme. Pass '=' to get the original encoding scheme.
   * s, pos, len, linelen: See the interface description of encode_substring.
   * first_linelen: The length of the first line.
   *
   * Returns: (s,last_linelen) where [s] is the encoded string, and 
   *   [last_linelen] is the length of the last line
   *)
  assert (Array.length b64 = 64);
  if len < 0 || pos < 0 || pos > Bytes.length s || linelen < 0 then
    invalid_arg "Netencoding.Base64.encode";
  if pos + len > Bytes.length s then
    invalid_arg "Netencoding.Base64.encode";

  let linelen = (linelen asr 2) lsl 2 in
  let first_linelen = (first_linelen asr 2) lsl 2 in

  let l_t = if len = 0 then 0 else ((len - 1) / 3 + 1) * 4 in
  (* l_t: length of the result without additional line endings *)

  let factor = if crlf then 2 else 1 in
  let l_t' = 
    if linelen < 4 then
      l_t
    else
      if l_t <= first_linelen then 
	( if l_t = 0 then 0 else l_t + factor )
      else 
	let n_lines = ((l_t - first_linelen - 1) / linelen) + 2 in
	l_t + n_lines * factor
  in
  (* l_t': length of the result with CRLF or LF characters *)
  
  let t = Bytes.make l_t' equal in
  let j = ref 0 in
  let q = ref (linelen - first_linelen) in
  for k = 0 to len / 3 - 1 do
    let p = pos + 3*k in
    (* p >= pos >= 0: this is evident
     * p+2 < pos+len <= Bytes.length s:
     *   Because k <= len/3-1
     *         3*k <= 3*(len/3-1) = len - 3
     *   pos+3*k+2 <= pos + len - 3 + 2 = pos + len - 1 < pos + len
     * So it is proved that the following unsafe string accesses always
     * work.
     *)
    let bits = (Char.code (Bytes.unsafe_get s (p))   lsl 16) lor
      (Char.code (Bytes.unsafe_get s (p+1)) lsl  8) lor
      (Char.code (Bytes.unsafe_get s (p+2))) in
    (* Obviously, 'bits' is a 24 bit entity (i.e. bits < 2**24) *)
    assert(!j + 3 < l_t');
    Bytes.unsafe_set t !j     (Array.unsafe_get b64 ( bits lsr 18));
    Bytes.unsafe_set t (!j+1) (Array.unsafe_get b64 ((bits lsr 12) land 63));
    Bytes.unsafe_set t (!j+2) (Array.unsafe_get b64 ((bits lsr  6) land 63));
    Bytes.unsafe_set t (!j+3) (Array.unsafe_get b64 ( bits         land 63));
    j := !j + 4;
    if linelen > 3 then begin
      q := !q + 4;
      if !q + 4 > linelen then begin
	(* The next 4 characters won't fit on the current line. So insert
	 * a line ending.
	 *)
	if crlf then begin
	  Bytes.set t !j '\013';
	  Bytes.set t (succ !j) '\010';
	  j := !j + 2;
	end
	else begin
	  Bytes.set t !j '\010';
	  incr j
	end;
	q := 0;
      end;
    end;
  done;
  (* padding if needed: *)
  let m = len mod 3 in
  begin
    match m with
	0 -> ()
      | 1 ->
          let bits = Char.code (s.[pos + len - 1]) in
	  Bytes.set t !j b64.(bits lsr 2);
	  Bytes.set t (succ !j) b64.((bits land 0x03) lsl 4);
	  j := !j + 4;
	  q := !q + 4;
      | 2 ->
	  let bits = (Char.code (s.[pos + len - 2]) lsl 8) lor
            (Char.code (s.[pos + len - 1])) in
	  Bytes.set t !j b64.( bits lsr 10);
	  Bytes.set t (succ !j) b64.((bits lsr  4) land 0x3f);
	  Bytes.set t (!j + 2)  b64.((bits lsl  2) land 0x3f);
	  j := !j + 4;
	  q := !q + 4;
      | _ -> assert false
  end;

  (* If required, add another line end: *)

  if linelen > 3 && !q > 0 && len > 0 then
    begin
      if crlf then 
	begin
	  Bytes.set t !j '\013';
	  Bytes.set t (succ !j) '\010';
	  j := !j + 2;
	end
      else 
	begin
	  Bytes.set t !j '\010';
	  incr j;
	end;	
    end;

  (t, !q) ;;

let encode ?(pos=0) ?len ?(linelength=0) ?(crlf=false) s =
  let l = match len with None -> Bytes.length s - pos | Some x -> x in
  let s,_ = 
    encode_with_options rfc_pattern '=' s pos l linelength linelength crlf in
  s

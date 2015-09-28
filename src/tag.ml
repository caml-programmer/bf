open Printf
open Logger

let mk (pkgname,ver,rev) =
  sprintf "%s/%s-%d" pkgname ver rev

let make pkgname ver rev =
  mk (pkgname,ver,rev)
	  
let pkgname tag =
  try
    Some (String.sub tag 0 (String.index tag '/'))
  with Not_found -> None

let of_specdir specdir =
  try
    let pkgname = Specdir.pkgname specdir in
    let (ver,rev) = Release.get specdir in
    Some (mk (pkgname, ver, rev))
  with exn -> log_message
    (sprintf "Warning: cannot parse release file from %s by error (%s)" specdir
      (Printexc.to_string exn));
    None

let parse tag =
  try
    let len = String.length tag in
    let pos = String.index tag '/' in
    let pkgname = String.sub tag 0 pos in
    let vr = String.sub tag (succ pos) (len - pos - 1) in
    let vrl = String.length vr in
    let pos2 = String.rindex vr '-' in
    Some (pkgname,
    (String.sub vr 0 pos2),
    (int_of_string (String.sub vr (succ pos2) (vrl - pos2 - 1))))
  with Not_found -> None

open Printf
open Logger

let mk (pkgname,ver,rev) =
  sprintf "%s/%s-%d" pkgname ver rev

let pkgname tag =
  try
    Some (String.sub tag 0 (String.index tag '/'))
  with Not_found -> None

let of_specdir specdir =
  try
    let pkgname = Specdir.pkgname specdir in
    let (ver,rev) = Release.read specdir in
    Some (mk (pkgname, ver, rev))
  with exn -> log_message
    (sprintf "Warning: cannot parse release file from %s by error (%s)" specdir
      (Printexc.to_string exn));
    None


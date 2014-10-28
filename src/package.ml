open Printf

exception Release_not_found of (string * exn)

let vr_compare a b =
  let r = compare (fst b) (fst a) in
  if r = 0 then
    compare (snd b) (snd a)
  else r

let max_vr l =
  List.hd (List.sort vr_compare l)
     
let release ?(next=false) ?version specdir =
  let with_next n = if next then succ n else n in
  let make s =
    let (ver,rev) =
      let pos = String.index s ' ' in
      String.sub s 0 pos,
      (with_next
	(int_of_string
	  (String.sub s (succ pos) (String.length s - pos - 1))))
    in (ver,rev)
  in
  let filter (v,r) =
    match version with
      | Some v' -> v' = v
      | None -> true
  in
  let file = Filename.concat specdir "release" in
  (try
    if Sys.file_exists file then
      let ch = open_in file in
      max_vr (List.filter filter
	(List.map make (System.list_of_channel ch)))
    else raise Exit
  with exn -> 
    raise (Release_not_found (specdir,exn)))


let string_of_pkgexn = function
  | Release_not_found (s,exn) ->
      sprintf "Package.Release_not_found(%s,%s)" s (Printexc.to_string exn)
  | exn -> Printexc.to_string exn

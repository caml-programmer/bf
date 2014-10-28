let pkgname specdir =
  Filename.basename (Filename.dirname specdir)

let branch s =
  Filename.basename s   

let get_version file =
  let s = System.read_file file in
  try
    String.sub s 0 (String.index s '\n')
  with Not_found -> s

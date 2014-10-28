open Types

exception Bad_specdir of string

let specdir specdir =
  try
    ignore(Specdir.pkgname specdir);
    ignore(Specdir.branch specdir);
    let p =
      Filename.basename
	(Filename.dirname 
	  (Filename.dirname specdir)) in
    if p <> "pack" && p <> "pack.git" then
      raise (Bad_specdir specdir)
  with _ ->
    raise (Bad_specdir specdir)

let pack_component () =
  let component = 
    Component.make ~label:(Branch "master") "pack" in
  ignore(Component.update_pack component);
  ignore
    (System.with_dir component.name
      (fun () ->
	(match Git.git_current_branch () with
	  | Some "master" -> ()
	  | _ -> Git.git_checkout ~force:true ~key:"master" ())))

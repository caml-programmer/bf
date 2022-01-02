open Component

exception Bad_specdir of string

let specdir specdir =
  try
    ignore(Specdir.pkgname specdir);
    ignore(Specdir.branch specdir);
    let p =
      Filename.basename
	(Filename.dirname 
	  (Filename.dirname specdir)) in
    let name =
      Params.get_param "pack" in
    if p <> name && p <> name ^ ".git" then
      raise (Bad_specdir specdir)
  with _ ->
    raise (Bad_specdir specdir)

(* Проверяет, что pack последней версии и на ветке master *)
let pack_component () =
  let component = 
    Component.make ~label:(Branch "master") (Params.get_param "pack") in
  ignore(Component.update_pack component);
  ignore
    (System.with_dir component.name
      (fun () ->
	(match Git.git_current_branch () with
	  | Some "master" -> ()
	  | _ -> Git.git_checkout ~force:true ~key:"master" ())))

let install_dir () =
  let install_dir =
    Filename.concat 
      (Params.get_param "dest-dir")
      (Params.get_param "top-dir") in
  if not (Sys.file_exists install_dir) then
    System.make_directory_r install_dir

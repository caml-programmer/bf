open Component
open Printf
open Logger

exception Pack_current_branch_is_not_set

let pack specdir component =
  let is_release s =
    let x = "release" in
    let sl = String.length s in
    let xl = String.length x in
    sl > xl && String.sub s (sl - xl) xl = x      
  in  
  let tag' = Tag.of_specdir specdir in
  System.with_dir component.name
    (fun () ->
      match Git.git_current_branch () with
	| Some cur ->
	    (match tag' with
		Some tag ->
		  let rex = Pcre.regexp
		    (sprintf "%s/%s/"
		      (Specdir.pkgname specdir)
		      (Specdir.branch specdir))
		  in
		  (try
		    List.exists (fun s ->
		      let changed =
			(Pcre.pmatch ~rex s) && not (is_release s) in
		      if changed then
			log_message (sprintf "%s is changed" s);
		      changed)
		      (Git.git_changes tag cur)
		  with Git.Key_not_found key ->
		    log_message (sprintf "Warning: git-key (%s) is not found in pack" key);
		    true)
	      | None -> 
		  log_message (sprintf "Warning: cannot find current tag by specdir(%s)" specdir);
		  true)
	| None -> raise Pack_current_branch_is_not_set)

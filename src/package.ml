open Printf
open Component
open Logger
open Spectype

let reinstalled_components = (* for update and upgrade actions *)
  Hashtbl.create 32;;

exception Cannot_build_package of string

let build (specdir,ver,rev,spec) =
  let specdir =
    System.path_strip_directory specdir in
  let pkgname = Specdir.pkgname specdir in
  let tag = (pkgname,ver,rev) in
  
  Check.specdir specdir;
    
  let components =
    if Params.get_param "use-external" = "true" then
      spec.components
    else
      Components.only_local spec.components in
  
  List.iter Component.fetch_tags components;

  if not (Components.tag_ready ~tag:(Tag.mk tag) components) then
    raise (Cannot_build_package (Tag.mk tag));
  ignore
    (Components.install (Components.with_tag (Some (Tag.mk tag))
      (* выкидываем pack-компонент, чтобы не было ненужных checkout'ов в pack'e *)
      (List.filter (fun c -> c.name <> Params.get_param "pack") components)));
  (try
    Pkgbuild.build_package_file ~ready_spec:(Some spec) (specdir,ver,string_of_int rev);
  with
    | Platform.Permanent_error s -> log_error s;
    | exn -> log_error (Printexc.to_string exn));
  true

exception Revision_must_be_digital of string

let with_pack components =
  let pack =
    Component.make
      ~label:(Branch "master")
      ~nopack:true (Params.get_param "pack") in
  pack::components

let update ~specdir
  ?(check_pack=true)
  ?(check_fs=false)
  ?(lazy_mode=false)
  ?(interactive=false)
  ?(top=false)
  ?(ver=None) ?(rev=None) () =
  let specdir = System.path_strip_directory specdir in
  Check.specdir specdir;
  if check_pack then
    Check.pack_component ();

  let pkgname = Specdir.pkgname specdir in
  let branch = Specdir.branch specdir in

  let have_pack_changes =
    Changes.pack specdir (Component.make ~label:(Branch "master") "pack") in

  let conv_revision r =
    try int_of_string r with _ -> raise (Revision_must_be_digital r) in

  let custom_revision = ref false in
  
  let (version,revision) =
    (try
      (match ver with
	| Some v ->
	    custom_revision := true;
	    v, (match rev with Some r -> conv_revision r | None -> log_error (sprintf "cannot update %s: revision does not set" pkgname))
	| None ->
	    Release.get ~next:true specdir)
    with Release.Release_not_found _ ->
      log_message (sprintf "Warning: Try using local pkg archive for search next package (%s %s) release" pkgname branch);
      let ver' =
	match ver with
	  | Some v -> v
	  | None -> Pkgsearch.version ~interactive pkgname
      in
      let rev' =
	match rev with
	  | Some r -> conv_revision r
	  | None -> succ (Pkgsearch.revision ~interactive pkgname ver')
      in (ver',rev'))
  in
  let have_fs_changes =
    if check_fs then
      begin
	let pat = 
	  sprintf "%s-%s-%d" pkgname version (pred revision) in
	let rex = Pcre.regexp pat in
	not (List.exists (Pcre.pmatch ~rex) (System.list_of_directory "."))
      end
    else false in
  let tag =
    (pkgname,version,revision) in
  
  let prev_tag =
    if revision > 0 then
      Some (pkgname,version,(pred revision))
    else None in
  
  let components =    
    let l =
      Composite.components
	(Filename.concat specdir "composite") in
    if Params.get_param "use-external" = "true" 
    then l else Components.only_local l in
  
  let have_composite_changes =
    Components.update components in
  
  let have_external_components_changes =
    List.exists
      (fun component -> 
	Hashtbl.mem reinstalled_components component.name)
      (Components.only_external components) in

  let add_reinstall c =
    Hashtbl.replace reinstalled_components c.name false in

  let force_rebuild c =
    log_message
      (sprintf "force %s rebuilding by external components changes" c.name);
    System.with_dir c.name
      (fun () ->
	if Sys.file_exists (Component.with_rules ".bf-build" c) then
	  Unix.unlink (Component.with_rules ".bf-build" c);
	if Sys.file_exists (Component.with_rules ".bf-install" c) then
	  Unix.unlink (Component.with_rules ".bf-install" c))
  in
  
  let build ?(prev=false) tag =
    let (pkgname,version,revision) = tag in

    if have_external_components_changes then
      List.iter force_rebuild (Components.only_local components);
    
    if not (Components.tag_ready ~tag:(Tag.mk tag) (with_pack components)) then
      begin
	List.iter add_reinstall (Components.install components);
	Components.make_tag (Tag.mk tag) (Components.only_local (with_pack components))
      end;
    
    List.iter add_reinstall
      (Components.install
	(Components.with_tag (Some (Tag.mk tag))
	  components));
    
    (try
      Pkgbuild.build_package_file (specdir,version,string_of_int revision);
      if not !custom_revision && not prev then
	ignore(Release.reg_pkg_release specdir version revision)
    with
      | Platform.Permanent_error s ->
	  if not !custom_revision && not prev then
	    ignore(Release.reg_pkg_release specdir version revision);
	  log_error s;
      | exn -> log_error (Printexc.to_string exn));
    
    (match prev_tag with
      | Some old ->
	  (try
	    if Params.get_param "smtp-notify-email" <> "" then
	      Components.changelog components (Tag.mk old) (Tag.mk tag)
	  with exn ->
	    log_message (Printexc.to_string exn))
      | None -> ());
    true
  in
  if lazy_mode && not have_composite_changes && not have_pack_changes then
    begin
      if have_fs_changes then
	match prev_tag with
	  | Some tag ->
	      log_message 
		(sprintf "pkg update (%s/%s): lazy-mode(%b), composite-changes(%b), pack-changes(%b), fs-changes(%b) -> previous-build(%s)"
		  pkgname branch lazy_mode have_composite_changes have_pack_changes have_fs_changes (Tag.mk tag));
	      build ~prev:true tag
	  | None ->
	      (log_message (sprintf "pkg update (%s/%s): nothing to do" pkgname branch);
	      false)
      else
	(log_message (sprintf "pkg update (%s/%s): nothing to do" pkgname branch);	
	false)
    end
  else
    begin    
      log_message 
	(sprintf "pkg update (%s/%s): lazy-mode(%b), composite-changes(%b), pack-changes(%b), fs-changes(%b) -> first-build(%s)"
	  pkgname branch lazy_mode have_composite_changes have_pack_changes have_fs_changes (Tag.mk tag));
      let result = build tag in
      if top then
	begin
	  (* Fixbuild support *)
	  (match prev_tag with
	    | None ->
		log_message "[fixbuild]: no prev-tag -> ignore fixbuild";
	    | Some (prev_pkgname, prev_ver, prev_rev) ->
		log_message "[fixbuild]: check jira-host";
		if Params.get_param "jira-host" <> "" then
		  (try
		    log_message (sprintf "[fixbuild]: jira-host=%s ready" (Params.get_param "jira-host"));
		    let (pkgname, ver, rev) = tag in
		    let rev_a = sprintf "%s-%d" prev_ver prev_rev in
		    let rev_b = sprintf "%s-%d" ver rev in
		    List.iter
		      (fun ((pkg',ver',rev'),tasks) ->
			if tasks = [] then
			  log_message (sprintf "[fixbuild]: no tasks found for %s/%s-%d" pkg' ver' rev');
			List.iter
			  (fun task_id ->
			    log_message (sprintf "[fixbuild]: fix %s, set build -> %s-%s-%d" task_id pkg' ver' rev');
			    Jira.fix_issue task_id (pkg',ver',rev');
			    log_message (sprintf "[fixbuild]: fix %s, set build -> %s-%s-%d" task_id pkgname ver rev);
			    Jira.fix_issue task_id (pkgname,ver,rev))
			  tasks)
		      (Fixmap.make specdir rev_a rev_b)
		  with exn ->
		    log_message (sprintf "[fixbuild]: error: %s" (Printexc.to_string exn)))
		else
		  log_message "[fixbuild]: jira-host empty -> ignore fixbuild")
	end;
      result
    end;



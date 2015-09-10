open Printf
open Types
open Logger
open Platform
open Spectype

let extract_packbranch ~userhost pkg_path =
  match
    (match userhost with
      | Some auth ->
	  (System.read_lines ~filter:(Pcre.pmatch ~rex:(Pcre.regexp "packbranch-"))
	    (sprintf "ssh %s rpm -qp --provides %s" auth pkg_path))
      | None ->
	  (System.read_lines ~filter:(Pcre.pmatch ~rex:(Pcre.regexp "packbranch-"))
	    (sprintf "rpm -qp --provides %s" pkg_path)))
  with [] -> raise (Pack_branch_is_not_found pkg_path)
    | hd::_ ->
	let pos = String.index hd '-' in
	let len =
	  try
	    String.index hd ' '
	  with Not_found -> String.length hd
	in
	String.sub hd (succ pos) (len - pos - 1)

let call_after_build ~snapshot ~location ~fullname hooks version release =
  Plugins.load ();
  (match hooks with
    | Some file -> Scheme.eval_file file
    | None -> ());
  let full_path =  sprintf "%s/%s" location fullname in
  let branch = extract_packbranch None full_path in
  if Scheme.defined "after-build" then
    Scheme.eval_code (fun _ -> ())
      (sprintf "(after-build \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" %s)"
	(System.hostname ()) location fullname version release branch
	(if snapshot then "#t" else "#f"))

let call_before_build ~snapshot ~pkgname ~version ~revision ~platform hooks =
  Plugins.load ();
  (match hooks with
    | Some file -> Scheme.eval_file file
    | None -> ());
  if Scheme.defined "before-build" then
    let result = ref [] in
    Scheme.eval_code (fun v ->
      result := Scheme.map Scheme.make_string v)
      (sprintf "(before-build \"%s\" \"%s\" \"%s\" \"%s\" %s)"
	pkgname version revision (string_of_platform platform)
	(if snapshot then "#t" else "#f"));
    !result
  else []
      
let build_over_rpmbuild ~snapshot params =
  let (pkgname,platform,version,release,spec,files,findreq,hooks) = params in
  let top_dir = Params.get_param "top-dir" in
  System.check_commands ["rpmbuild"];
  log_command "chmod" ["+x";findreq];
  Rpm.copy_to_buildroot ~top_dir files;
  let (location,fullname) =
    Rpm.build
      ~top_dir
      ~pkgname ~platform ~version ~release
      ~spec ~files ~findreq ()
  in call_after_build ~snapshot ~location ~fullname hooks version release

let check_composite_depends spec =
  let composite_depends =
    List.map
      (fun c ->
	match c.pkg with
	  | Some pkg -> pkg
	  | None -> assert false)
      (List.filter
	(fun c -> c.pkg <> None && (not c.nopack))
	spec.components)
  in
  let spec_depends =
    List.map 
      (fun (pkg_name,_,_) -> pkg_name) spec.depends
  in
  let rec make acc = function
    | [] -> acc
    | hd::tl ->
	if not (List.mem hd spec_depends) then
	  make (hd::acc) tl
	else
	  make acc tl
  in
  let missings = make [] composite_depends in
  if missings <> [] then
    begin
      List.iter 
	(fun pkg ->
	  log_message (sprintf "package (%s) is missing in depends file" pkg))
	missings;
      raise (Permanent_error "you must correct depends or composite files")
    end

let resolve_params find s =
  Pcre.substitute
    ~rex:(Pcre.regexp "%\\(.*?\\)")
    ~subst:(fun s ->
      let l = String.length s in
      let k = String.sub s 2 (l - 3) in
      (try find k with Not_found -> s))
    s

let build_package_impl ?(ready_spec=None) ?(snapshot=false) os platform (specdir,version,release) =
  let abs_specdir =
    if String.length specdir > 0 && specdir.[0] = '/' then
      specdir
    else
      Filename.concat (Sys.getcwd()) specdir
  in
  let with_specdir = Filename.concat abs_specdir in
  let with_out s f =
    let n = with_specdir s in
    let ch = open_out n in
    f (output_string ch);
    close_out ch; n
  in
  (match Specdir.get_version (with_specdir "version") with
    | "1.0" ->
	(match Spectype.load_v1 abs_specdir with
	    [spec;files;findreq] ->
	      let pkgname = 
		Filename.basename specdir in
	      let hookfile =
		Filename.concat abs_specdir "hooks.scm" in
	      let hooks =
		if Sys.file_exists hookfile then
		  Some hookfile
		else None
	      in build_over_rpmbuild ~snapshot
		   (pkgname,platform,version,release,spec,files,findreq,hooks)
	  | _-> assert false)
    | "2.0" 
    | "3.0" as specver ->
	let spec =
	  match ready_spec with
	    | Some s -> s
	    | None ->
		if specver = "3.0" then
		  Spectype.load_v3 ~snapshot ~version ~revision:release abs_specdir
		else
		  Spectype.load_v2 ~snapshot ~version ~revision:release abs_specdir in
	let bf_table = Hashtbl.create 32 in
	let reg k =
	  if Hashtbl.mem bf_table k then "" 
	  else 
	    begin
	      Hashtbl.add bf_table k false;
	      k
	    end
	in
	let accumulate_lists add out =
	  List.iter
	    (fun c ->
	      let name = c.name in
	      let bf_list =
		match c.rules with
		  | Some alt ->
		      Filename.concat name (sprintf ".bf-list.%s" alt)
		  | None ->
		      Filename.concat name ".bf-list"
	      in
	      let rec add_with_check () =
		if Sys.file_exists bf_list then
		  add out bf_list
		else
		  (if Params.get_param "autopkg" <> "false" then
		    begin
		      log_message
			(sprintf "bf list for (%s) is not found -> need installing" name);
		      let tag =
			let k =
			  Tag.mk ((Specdir.pkgname abs_specdir), version, (int_of_string release)) in
			let tag_exists = ref false in
			ignore(Component.with_component_dir ~strict:false c
			  (fun () -> 
			    tag_exists := List.mem k (Git.git_tag_list ())));
			if !tag_exists then
			  Some k
			else None
		      in
		      Components.reinstall (Components.with_tag tag [c]);
		      add_with_check ()
		    end
		  else
		    log_error (sprintf "bf list for (%s) is not found. Check your .bf-params and other configurations." name))
	      in add_with_check ())
	    (List.filter
	      (fun c -> c.pkg = None && (not c.nopack))
	      spec.components)
	in

	Depends.print spec.depends;
	check_composite_depends spec;
	
	let add_bf_list custom out file =
	  let ch = open_in file in
	  let rec read () =
	    try
	      out (custom (input_line ch));
	      read ()
	    with End_of_file -> close_in ch
	  in read ()
	in
	
	(match engine_of_platform platform with
	  | Rpm_build ->
	      let custom_pkg_files =
		call_before_build ~snapshot
		  ~pkgname:spec.pkgname ~version ~revision:release ~platform spec.hooks in
	      
	      let files =
		with_out "rpmbuild.files"
		  (fun out ->
		    let make_rpm_line s =
		      let l = String.length s in
		      if l > 2 then			      
			(match s.[0] with
			  | 'd' -> reg (sprintf "%%dir %s\n" (String.sub s 2 (l - 2)))
			  | 'f' -> reg (sprintf "%s\n" (String.sub s 2 (l - 2)))
			  | _ -> "")
		      else ""
		    in
		    accumulate_lists (add_bf_list make_rpm_line) out;
		    List.iter (fun s -> out (make_rpm_line s))
		      custom_pkg_files)
	      in
	      let findreq =
		with_out "rpmbuild.findreq"
		  (fun out -> 
		    out "#!/bin/sh\n";
		    List.iter 
		      (fun (pkg_name,ov_opt,_) ->
			(match ov_opt with
			  | Some (op,ver) ->
			      out (sprintf "echo \"%s %s %s\"\n"
				pkg_name (string_of_pkg_op op) ver)
			  | None ->
			      out (sprintf "echo %s\n" pkg_name)))
		      spec.depends;
		    let freq =
		      "/usr/lib/rpm/find-requires" in
		    if Sys.file_exists freq then
		      begin
			out "/usr/lib/rpm/find-requires";
			out "\\\n| grep -v ^$";
			List.iter
			  (fun reject ->
			    out (sprintf "\\\n| sed -e 's#%s##'" reject))
			  spec.rejects
		      end
		    else
		      log_message (sprintf "warning: %s is not found" freq))
	      in
	      let specify_provides l =
		if System.arch () = "x86_64" then
		  List.map
		    (fun p ->
		      if Pcre.pmatch ~rex:(Pcre.regexp "^lib") p then
			p ^ "()(64bit)"
		      else p) l
		else l
	      in		    
	      let specfile =
		with_out "rpmbuild.spec"
		  (fun out ->
		    let find_value = function
		      | "topdir" -> Params.get_param "top-dir"
		      | "prefix" -> Params.get_param "top-dir"
		      | "name" -> spec.pkgname
		      | "version" -> version
		      | "release" -> release ^ "." ^ (string_of_platform platform)
		      | "buildroot" -> "buildroot"
		      | "provides" ->
			  String.concat ", " (specify_provides spec.provides)
		      | "obsoletes" ->
			  String.concat ", " spec.obsoletes
		      | k -> Hashtbl.find spec.params k
		    in
		    let gen_param k =
		      (try
			out (sprintf "%s: %s\n" (Rpm.key_format k) (find_value k))
		      with Not_found -> ())
		    in
		    gen_param "summary";
		    gen_param "name";
		    gen_param "version";
		    gen_param "release";
		    gen_param "license";
		    gen_param "vendor";
		    gen_param "group";
		    gen_param "url";
		    gen_param "buildroot";
		    gen_param "prefix";
		    if spec.provides <> [] then
		      gen_param "provides";
		    if spec.obsoletes <> [] then
		      gen_param "obsoletes";
		    
		    out "%define _use_internal_dependency_generator 0\n";
		    out "%define __find_requires %findreq\n";
		    out "%description\n";
		    out "%files\n";
		    out (sprintf "%%include %s\n" files);

		    let oo s = out (resolve_params find_value s); out "\n" in
		    
		    (match spec.pre_install with
		      | None -> ()
		      | Some pre ->
			  out "%pre\n";
			  out "if [ \"$1\" = \"1\" ] ; then # first install\n";
			  out "echo -n\n";
			  oo pre;
			  out "fi\n";
			  (match spec.pre_update with
			    | None -> ()
			    | Some preup ->
				out "if [ \"$1\" = \"2\" ] ; then # update\n";
				out "echo -n\n";
				oo preup;
				out "fi\n"));
		    (match spec.post_install with
		      | None -> ()
		      | Some post ->
			  out "%post\n";
			  oo post);
		    (match spec.pre_uninstall with
		      | None -> ()
		      | Some preun ->
			  out "%preun\n";
			  out "if [ \"$1\" = \"0\" ] ; then # all versions deleted\n";
			  out "echo -n\n";
			  oo preun;
			  out "fi\n"))
	      in
	      
	      build_over_rpmbuild ~snapshot
		(spec.pkgname,platform,version,release,specfile,files,findreq,spec.hooks)
		
	  | Pkg_trans ->
	      let custom_pkg_files =
		call_before_build ~snapshot
		  ~pkgname:spec.pkgname ~version ~revision:release ~platform spec.hooks in
	      
	      let find_value = function
		| "topdir" -> Params.get_param "top-dir"
		| "pkg" -> 
		    Pkgtrans.name_format spec.pkgname
		| "arch" -> System.arch ()
		| "version" -> sprintf "%s-%s" version release
		| "category" -> Hashtbl.find spec.params "group"
		| "name" -> Hashtbl.find spec.params "summary"
		| k -> Hashtbl.find spec.params k
	      in
	      let _ =
		with_out "pkginfo"
		  (fun out -> 
		    let gen_param k =
		      (try
			out (sprintf "%s=%s\n" (Pkgtrans.key_format k) (find_value k))
		      with Not_found -> ())
		    in
		    gen_param "pkg";
		    gen_param "arch";
		    gen_param "name";
		    gen_param "version";
		    gen_param "vendor";
		    gen_param "category";
		    gen_param "email")
	      in
	      let _ =
		with_out "prototype"
		  (fun out ->
		    let make_pkgtrans_line s =
		      let l = String.length s in
		      if l > 2 then
			(match s.[0] with
			  | 'd' ->
			      let dir = String.sub s 2 (l - 2) in
			      let mode = sprintf "%o" (Unix.stat dir).Unix.st_perm in
			      reg (sprintf "d none %s %s root root\n" dir mode)
			  | 'f' -> 
			      let file = String.sub s 2 (l - 2) in
			      let mode = sprintf "%o" (Unix.stat file).Unix.st_perm in
			      reg (sprintf "f none %s %s root root\n" file mode)
			  | _ -> "")
		      else ""
		    in
		    let write_content name content =
		      let file =
			Filename.concat abs_specdir name in
		      out (sprintf "i %s=%s\n" name file);
		      System.write_string
			~file ~string:(resolve_params find_value content)
		    in
		    let read_depends depends =
		      let b = Buffer.create 32 in
		      let out = Buffer.add_string b in
		      List.iter 
			(fun (pkg_name,_,pkg_desc_opt) ->
			  let pkg_desc =
			    match pkg_desc_opt with
			      | None -> pkg_name
			      | Some s -> s
			  in out (sprintf "P %s %s\n" (Pkgtrans.name_format pkg_name) pkg_desc))
			depends;
		      Buffer.contents b
		    in
		    
		    out (sprintf "i pkginfo=%s/pkginfo\n" abs_specdir);
		    
		    (match spec.pre_install with
		      | None -> ()
		      | Some content ->
			  (match spec.pre_update with
			    | None ->
				write_content "preinstall" content
			    | Some upd ->
				write_content "preinstall"
				  (content ^ "\n" ^  upd)));
		    (match spec.post_install with
		      | None -> ()
		      | Some content ->
			  write_content "postinstall" content);
		    (match spec.pre_uninstall with
		      | None -> ()
		      | Some content ->
			  write_content "preremove" content);
		    (match spec.depends with
		      | [] ->  ()
		      | list ->
			  write_content "depend" (read_depends list));
		    
		    accumulate_lists (add_bf_list make_pkgtrans_line) out;
		    List.iter (fun s -> out (make_pkgtrans_line s))
		      custom_pkg_files)
	      in
	      
	      let pkg_spool = "/var/spool/pkg/" in
	      let pkg_name = find_value "pkg" in
	      let pkg_file = sprintf "%s-%s.%s.%s" 
		pkg_name (find_value "version")
		(string_of_platform platform)
		(find_value "arch")
	      in
	      let pkg_file_abs = Filename.concat pkg_spool pkg_file in
	      let pkg_file_gz = pkg_file ^ ".gz" in
	      
	      Commands.remove_directory (Filename.concat pkg_spool pkg_name);

	      System.with_dir abs_specdir
		(fun () ->
		  let root =
		    match Params.dest_dir () with
		      | Some d -> d
		      | None -> "/" in
		  log_command "pkgmk"
		    ["-o";"-r";root];
		  log_command "pkgtrans" 
		    ["-o";"-s";pkg_spool; pkg_file; pkg_name]);
	      
	      log_command "mv" ["-f";pkg_file_abs;"./"];
	      (try Sys.remove pkg_file_gz with _ -> ());
	      log_command "gzip" [pkg_file];
	      call_after_build ~snapshot
		~location:(Sys.getcwd ())
		~fullname:pkg_file_gz spec.hooks version release
	  | Deb_pkg ->
	      let custom_pkg_files =
		call_before_build ~snapshot
		  ~pkgname:spec.pkgname ~version ~revision:release ~platform spec.hooks in
	      
	      let make_debian_depends deps =
		let b = Buffer.create 32 in
		let add s =
		  if Buffer.length b = 0 then
		    Buffer.add_string b s
		  else
		    begin
		      Buffer.add_string b ", ";
		      Buffer.add_string b s
		    end
		in
		List.iter
		  (fun (pkgname, ov_opt, _) ->
		    match ov_opt with
		      | Some (op,ver) ->
			  add (sprintf "%s (%s %s)" pkgname (string_of_pkg_op op) ver)
		      | None ->
			  add pkgname)
		  deps;
		Buffer.contents b
	      in
	      let find_value = function
		| "topdir" -> Params.get_param "top-dir"
		| "source" -> spec.pkgname
		| "package" -> spec.pkgname
		| "priority" -> "optional"
		| "maintainer" -> Hashtbl.find spec.params "email"
		| "architecture" -> Debian.fix_arch (System.arch ())
		| "version" -> sprintf "%s-%s" version release
		| "section" -> Hashtbl.find spec.params "group"
		| "description" -> Hashtbl.find spec.params "summary"
		| "depends" -> make_debian_depends spec.depends
		| k -> Hashtbl.find spec.params k
	      in
	      
	      let make_date () =
		let tm = Unix.localtime (Unix.time ()) in
		sprintf "%04d-%02d-%02d" 
		  (tm.Unix.tm_year + 1900)
		  (tm.Unix.tm_mon + 1)
		  (tm.Unix.tm_mday)
	      in
	      
	      let make_deb_line s =
		let l = String.length s in
		if l > 2 then
		  (match s.[0] with
		    | 'd' ->
			let dir =
			  Filename.concat 
			    (Filename.concat abs_specdir "debian")
			    (System.strip_root (String.sub s 2 (l - 2))) in
			Commands.make_directory [dir];
		    | 'f' ->
			let src = String.sub s 2 (l - 2) in
			let dst = Filename.concat 
			  (Filename.concat abs_specdir "debian") 
			  (System.strip_root src) in
                        let dir = Filename.dirname dst in
			Commands.make_directory [dir];
			System.link_or_copy
			  (match Params.dest_dir () with
			    | Some d -> Filename.concat d src
			    | None -> src) dst
		    | _ -> ())
	      in
	      let debian_home =
		Filename.concat abs_specdir "debian" in
	      Commands.remove_directory debian_home;
	      Commands.make_directory [debian_home];
	      accumulate_lists (add_bf_list make_deb_line) (fun _ -> ());
	      List.iter make_deb_line custom_pkg_files;
	      
	      let write_script name content =
		let file =
		  Filename.concat
		    (Filename.concat abs_specdir "debian/DEBIAN") name in
		System.write_string
		  ~file ~string:(resolve_params find_value (sprintf "#!/bin/sh\n%s\n" content));
		Unix.chmod file 0o755
	      in

	      System.with_dir abs_specdir
		(fun () ->
		  let doc_location =
		    sprintf "debian/usr/share/doc/%s" spec.pkgname in
		  let man_location =
		    sprintf "debian/usr/share/man/man1" in
		  
		  Commands.make_directory [
		    "debian/DEBIAN";
		    man_location;
		    doc_location;
		  ];
		  
		  (match spec.pre_install with
		    | None -> ()
		    | Some content ->
			(match spec.pre_update with
			  | None ->
			      write_script "preinst" content
			  | Some upd ->
			      write_script "preinst" (content ^ "\n" ^  upd)));
		  (match spec.post_install with
		    | None -> ()
		    | Some content ->
			write_script "postinst" content);
		  (match spec.pre_uninstall with
		    | None -> ()
		    | Some content ->
			write_script "prerm" content);

		  let _ =
		    with_out (Filename.concat doc_location "copyright")
		      (fun out ->
			out (find_value "email"))
		  in			
		  let manpage =
		    with_out
		      (Filename.concat man_location (spec.pkgname ^ ".1"))
		      (fun out ->
			out (sprintf ".TH %s 1 \"%s\"\n" spec.pkgname (make_date ()));
			out ".SH NAME\n";
			out (sprintf "%s - debian package\n" spec.pkgname);
			out ".SH DESCRIPTION\n";
			out (find_value "description");
			out "\n";
			out ".SH AUTHOR\n";
			out (find_value "email");
			out "\n")
		  in
		  let _ =
		    with_out "debian/DEBIAN/control"
		      (fun out ->
			let gen_param k =
			  (try
			    out (sprintf "%s: %s\n" (Rpm.key_format k) (find_value k))
			  with Not_found -> ())
			in
			gen_param "source";
			gen_param "section";
			gen_param "priority";
			gen_param "maintainer";
			gen_param "version";
			gen_param "package";
			gen_param "architecture";
			gen_param "depends";
			gen_param "description";
			out
			  (" " ^ (find_value "description") ^ "\n"))
		  in
		  let changelog =
		    with_out (Filename.concat doc_location "changelog")
		      (fun out ->
			out (sprintf "%s (%s-%s) unstable; urgency=low\n" spec.pkgname version release);
			out "\n";
			out " * Current Release.\n";
			out "\n";
			out
			  (sprintf "-- %s %s\n" 
			    (find_value "email")
			    (List.hd (System.read_lines "date -R"))))
		  in
		  let changelog_deb =
		    with_out (Filename.concat doc_location "changelog.Debian")
		      (fun out ->
			out (sprintf "%s (%s-%s) unstable; urgency=low\n" spec.pkgname version release);
			out "\n";
			out " * Current Release.\n";
			out "\n";
			out
			  (sprintf "-- %s %s\n"
			    (find_value "email")
			    (List.hd (System.read_lines "date -R"))))
		  in
		  
		  log_command "gzip" ["--best";manpage];
		  log_command "gzip" ["--best";changelog];
		  log_command "gzip" ["--best";changelog_deb];
		  log_command "fakeroot" ["dpkg-deb";"--build";"debian"]);
	      
	      let pkgfile =
		sprintf "%s-%s-%s.%s.deb" 
		  spec.pkgname version release (Debian.fix_arch (System.arch ())) in
	      log_command
		"mv" [(Filename.concat abs_specdir "debian.deb");pkgfile])
    | version ->
	raise (Spectype.Unsupported_specdir_version version))

let build_package_file ?(snapshot=false) ?(ready_spec=None) args =
  with_platform
    (fun os platfrom ->
      build_package_impl ~snapshot ~ready_spec os platfrom args)

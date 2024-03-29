open Deptree
open Printf
open Logger

type upgrade_mode =
  | Full
  | Lazy
  | Complete
  | Default

let mark_table_file =
  ".bf-upgrade-marktable"

let create_mark_table () =
  let table = Hashtbl.create 32 in
  if Sys.file_exists mark_table_file then
    begin
      let ch = open_in mark_table_file in
      (try
	while true do
	  Hashtbl.replace
	    table (input_line ch) true
	done
      with End_of_file -> close_in ch)
    end;
  ((open_out mark_table_file), table)

let make specdir upgrade_mode default_branch =
  let specdir = System.path_strip_directory specdir in
  let pkgname = Specdir.pkgname specdir in

  Params.update_for_specdir specdir;
  
  Check.install_dir ();
  Check.specdir specdir;
  Check.pack_component ();

  let deptree =
    log_message "make depends tree...";
    Packtree.create ~default_branch specdir in
  
  let depends = 
    list_of_deptree 
      ~add_parent:true (map_deptree fst deptree) in
  log_message "depend list...";
  List.iter print_endline depends;
  
  Interactive.stop_delay (int_of_string (Params.get_param "stop-delay"));

  let build_table = Hashtbl.create 32 in
  let (mark_channel, mark_table) = create_mark_table () in

  let find_specdir specdir =
    let rec make acc = function
      | Dep_val ((specdir',vr_opt), Dep_list l) ->
	  let new_acc = specdir'::acc in
	  if specdir = specdir' then
	    begin
	      match vr_opt with
		| Some (ver,rev_opt) ->
		    (match rev_opt with
		      | Some _ ->
			  acc :: (List.flatten (List.map (make new_acc) l))
		      | None -> [])
		| None -> []
	    end
	  else
	    List.flatten (List.map (make new_acc) l)
      | _ -> assert false
    in List.flatten (make [] deptree)
  in

  let eval_lazy_mode specdir =
    let dep_paths =
      find_specdir specdir in
    if Hashtbl.mem mark_table specdir && not (Hashtbl.mem build_table specdir) then
      (false,dep_paths)
    else
      (true,dep_paths)
  in
  
  let complete_impl check_fs_packages =
    List.iter
      (fun specdir ->
	let (lazy_mode,dep_paths) =
	  eval_lazy_mode specdir in
	log_message (sprintf "lazy-mode is %b for %s, dep-paths:" lazy_mode specdir);
	List.iter log_message dep_paths;
	let updated =
	  Package.update
	    ~specdir
	    ~lazy_mode
	    ~top:(Specdir.pkgname specdir = pkgname)
	    ~check_pack:false
	    ~check_fs:check_fs_packages
	    ~interactive:true () in
	Hashtbl.replace build_table specdir updated;
	if updated then
	  List.iter
	    (fun s ->
	      output_string mark_channel (s ^ "\n");
	      flush mark_channel;
	      Hashtbl.replace mark_table s true)
	    dep_paths)
      depends
  in
  
  (match upgrade_mode with
    | Full ->
	List.iter
	  (fun specdir ->
	    ignore(Package.update ~specdir ~check_pack:false ~lazy_mode:false ~interactive:true ()))
	  depends
    | Lazy ->
	List.iter
	  (fun specdir ->
	    ignore(Package.update ~specdir ~check_pack:false ~lazy_mode:true ~interactive:true ()))
	  depends
    | Complete ->
	complete_impl false;
    | Default ->
	complete_impl true);  

  if Sys.file_exists mark_table_file then
    Unix.unlink mark_table_file

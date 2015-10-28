open Printf
open Logger
open Deptree

(* Snapshot *)

let make_snapshot_id () =
  let t = Unix.localtime (Unix.time ()) in
  let ver = 
    sprintf "SS%04d%02d%02d"
      (t.Unix.tm_year+1900)
      (t.Unix.tm_mon+1)
      t.Unix.tm_mday in
  let rev =
    sprintf "%02d%02d%02d"
      (t.Unix.tm_hour)
      (t.Unix.tm_min)
      t.Unix.tm_sec in
  (ver, rev)

let make ?(composite=None) specdir =
  let specdir =
    System.path_strip_directory specdir in
  let (ver,rev) =
    make_snapshot_id () in
  Check.specdir specdir;
  Check.pack_component ();
  let toptree =
    log_message "make depends tree...";
    Top.tree_of_specdir specdir in
  let depends = list_of_deptree toptree in
  log_message "depend list...";
  List.iter (fun (specdir,_,_) -> print_endline specdir) depends;
  Interactive.stop_delay 5;
  Top.make ~replace_composite:composite ~depends specdir;
  List.iter
    (fun (specdir,_,_) ->
     ignore (Pkgbuild.build_package_file ~snapshot:true (specdir,ver,rev)))
    depends

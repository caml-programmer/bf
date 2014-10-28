open Deptree
open Printf

exception Cannot_create_image of string
exception Cannot_view_image of string

let monograph ?ver ?rev specdir =
  let dotfile = "graph.dot" in
  let pngfile = "graph.png" in

  let vr =
    match ver,rev with
      | Some v, Some r -> Some (v,r)
      | _ -> None
  in

  let tree = Clone.tree_of_specdir ~vr specdir in
  let depends =  
    list_of_deptree tree in
  
  List.iter 
    (fun (n,v,r,_) -> printf "%s %s %d\n" (Specdir.pkgname n) v r) depends;
  let ch = open_out dotfile in
  let out = output_string ch in
  
  out "digraph g {\n";
  out "graph [ rankdir = \"LR\" ];\n";
  out "node  [ fontname = \"Arial\", fontsize = \"10\" ];\n"; (* , //shape = record  *)
  out "edge  [ labelfontname = \"Arial\", labelfontsize = \"10\" ];\n";  (* , //style = dashed *)
  
  List.iter
    (fun (n,v,r,_) ->
      out (sprintf "\"%s\\n%s-%d\"
    [shape=box,style=\"rounded,filled\",fillcolor=\"#77CC77\"]\n" (Specdir.pkgname n) v r))
    depends;

  let rec write_links parent = function
    | Dep_val (e, tree) ->
	let (en,ev,er,_) = e in
	(match parent with
	  | Some p ->
	      let (pn,pv,pr,_) = p in
	      out (sprintf "\"%s\\n%s-%d\" -> \"%s\\n%s-%d\"\n" 
		(Specdir.pkgname pn) pv pr 
		(Specdir.pkgname en) ev er);
	      write_links (Some e) tree
	  | None ->
	      write_links (Some e) tree)
    | Dep_list l ->
	List.iter (write_links parent) l
  in

  let make_image () =
    if Sys.command (sprintf "dot -Tpng %s > %s" dotfile pngfile) <> 0 then
      raise (Cannot_create_image pngfile)
  in
  
  let view_image () =
    if Sys.command (sprintf "qiv -f %s" pngfile) <> 0 then
      if Sys.command (sprintf "gqview %s" pngfile) <> 0 then
	raise (Cannot_view_image pngfile)
  in

  write_links None tree;
  
  out "}\n";
  close_out ch;
  make_image ();
  view_image ()

let basegraph specdir mode =
  let dotfile = "graph.dot" in
  let pngfile = "graph.png" in

  let tree = Packtree.create ~default_branch:(Some (Specdir.branch specdir)) specdir in
  let depends =
    list_of_deptree (map_deptree fst tree) in

  let ch = open_out dotfile in
  let out = output_string ch in
  
  out "digraph g {\n";
  out "graph [ rankdir = \"LR\" ];\n";
  out "node  [ fontname = \"Arial\", fontsize = \"10\" ];\n"; (* , //shape = record  *)
  out "edge  [ labelfontname = \"Arial\", labelfontsize = \"10\" ];\n";  (* , //style = dashed *)
  
  let string_of_vr = function
    | Some (ver,rev_opt) ->
	sprintf "%s%s" ver (match rev_opt with Some rev -> sprintf "-%d" rev  | None -> "")
    | None -> "any"
  in
  
  List.iter
    (fun n ->
      out (sprintf "\"%s\" 
                    [shape=box,style=\"rounded,filled\",fillcolor=\"#77CC77\"]\n" (Specdir.pkgname n)))
    depends;

  let rec write_links parent = function
    | Dep_val (e, tree) ->
	let (en,evr_opt) = e in
	(match parent with
	  | Some p ->
	      let (pn,pvr_opt) = p in
	      if Version.have_revision evr_opt then
		if mode = "full" || mode = "hard" then
		  out (sprintf "\"%s\" -> \"%s\" [label=\"%s\", color=\"black\"]\n"
		    (Specdir.pkgname pn)
		    (Specdir.pkgname en)
		    (string_of_vr evr_opt))
		else ()
	      else
		if mode = "full" || mode = "soft" then
		  out (sprintf "\"%s\" -> \"%s\" [label=\"%s\", color=\"black\", style=\"dashed\"]\n"
		    (Specdir.pkgname pn)
		    (Specdir.pkgname en)
		    (string_of_vr evr_opt))
		else ();
	      write_links (Some e) tree
	  | None ->
	      write_links (Some e) tree)
    | Dep_list l ->
	List.iter (write_links parent) l
  in

  let make_image () =
    if Sys.command (sprintf "dot -Tpng %s > %s" dotfile pngfile) <> 0 then
      raise (Cannot_create_image pngfile)
  in
  
  let view_image () =
    if Sys.command (sprintf "qiv -f %s" pngfile) <> 0 then
      if Sys.command (sprintf "gqview %s" pngfile) <> 0 then
	raise (Cannot_view_image pngfile)
  in

  write_links None tree;
  
  out "}\n";
  close_out ch;
  make_image ();
  view_image ()

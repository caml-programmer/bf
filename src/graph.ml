open Deptree
open Printf

exception Cannot_create_image of string
exception Cannot_view_image of string

let make_image dotfile pngfile =
  if Sys.command (sprintf "dot -Tpng %s > %s" dotfile pngfile) <> 0 then
    raise (Cannot_create_image pngfile)

let view_image pngfile =
  let ignore_view_image =
    try
      ignore(Sys.getenv "IGNORE_VIEW_IMAGE");
      true
    with Not_found -> false in

  if not ignore_view_image then
    if Sys.command (sprintf "qiv -f %s" pngfile) <> 0 then
      if Sys.command (sprintf "gqview %s" pngfile) <> 0 then
	raise (Cannot_view_image pngfile)

let graph_home =
  let get () =
    let dir = Params.get_param "graph-home" in
    if dir <> "" && (not (Sys.file_exists dir)) then
      System.make_directory_r dir;
    dir in
  lazy (get ())

let make_id typ specdir =
  let rex = Pcre.regexp "/" in
  match List.rev (Pcre.split ~rex specdir) with
    | branch::pkgname::_ ->
	Filename.concat (Lazy.force graph_home) (sprintf "%s-%s-%s" typ pkgname branch)
    | _ -> "graph"
  

(* Monograph *)

let monograph ?ver ?rev specdir =
  let dotfile = sprintf "%s.dot" (make_id "monograph" specdir) in
  let pngfile = sprintf "%s.png" (make_id "monograph" specdir) in

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

  write_links None tree;
  
  out "}\n";
  close_out ch;
  make_image dotfile pngfile;
  view_image pngfile

(* Basegraph *)

let basegraph specdir mode =
  let dotfile = sprintf "%s.dot" (make_id "basegraph" specdir) in
  let pngfile = sprintf "%s.png" (make_id "basegraph" specdir) in

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


  write_links None tree;
  
  out "}\n";
  close_out ch;
  make_image dotfile pngfile;
  view_image pngfile

(* Usergraph *)

let use_loc =
  try
    ignore (Sys.getenv "NOT_EVAL_LOC");
    false
  with _ -> true

let list_of_table table =
  let l =
    Hashtbl.fold
      (fun k v acc ->
	(k,v)::acc) table [] in
  (*
  let commit_sum =
    List.fold_left
      (fun acc v ->
	(fst v) + acc) 0 (List.map snd l) in
  let loc_sum =
    List.fold_left
      (fun acc v ->
	(snd v) + acc) 0 (List.map snd l) in
  *)
  List.map
    (fun (k,(commits,loc)) ->
      k,
      (*(100.0 *. (float_of_int (fst v) /. float_of_int commit_sum)),*)
      commits,
      loc)
    (List.sort (fun a b -> 
      if use_loc then
	compare (snd (snd b)) (snd (snd a))
      else 
	compare (fst (snd b)) (fst (snd a))) l)

let synonyms =
  Synonyms.load ()

let synonyms_patterns =
  List.map
    (fun (k,l) ->
      k, List.map
	(fun s ->
	  (try
	    Some (Pcre.regexp s)
	  with _ -> None)) l)
    synonyms

let rematch x l =
  try
    List.exists 
      (fun p_opt ->
	(match p_opt with
	  | Some rex ->
	      Pcre.pmatch ~rex x
	  | None -> false)) l
  with _ -> false

let resolve_synonyms x =
  List.fold_left2
    (fun acc (k,l) (_,pl) ->
      if List.mem x l || rematch x pl then
	k
      else
	acc)
    x synonyms synonyms_patterns

let extract_email s =
  let rex = Pcre.regexp "<([^>]*)>" in
  if Pcre.pmatch ~rex s then
    Some (Pcre.extract ~rex s).(1)
  else None

let loc_split s =
  let rex = Pcre.regexp "\\s+" in
  match Pcre.split ~rex s with
    | [count; commiter]
    | [_ ;count; commiter]
    | [_ ;count; commiter; _] ->
	begin
	  match extract_email commiter with
	    | Some commiter ->
		Some (commiter, int_of_string count)
	    | None -> None
	end
    | _ -> None

let loc_count () =
  let code_exts =
    (*".ml|.mli|types|values|.cc|.c|.h|.hh|.cpp|.scala|.java|.rkt|.ss|.erl|.scm|.py|.pl|.js|Makefile|.bf-rules"*)
    ".png|.jpeg|.html|.htm|.css" in
  
  List.fold_left
    (fun acc line ->
      match loc_split line with
	| Some (commiter, loc) -> 
	    (commiter,loc)::acc
	| None -> acc) [] 
    (System.read_lines
      (sprintf "git ls-files | grep -i -v -E '(%s)$' | xargs -n1 git blame -e -w | perl -ne '/^.*?\\((.*?)\\s+[\\d]{4}/; print $1,\"\\n\"' | sort -f | uniq -c" code_exts))

let commiters component =
  let table = Hashtbl.create 32 in

  let search () =
    let ignored = ref false in
    let loctable =      
      match component.Types.name with
	| "boost" | "icu" | "samba" | "squid" | "apache" | "ant" | "maven" -> 
	    ignored := true;
	    []
	| _ -> 
	    if use_loc then
	      loc_count ()
	    else []
    in
    let search_loc commiter =
      try
	Some (List.assoc commiter loctable)
      with Not_found -> 
	if !ignored then
	  None 
	else Some 0 in
    
    let commiters =
      List.fold_left 
	(fun acc commiter ->
	  match extract_email commiter with
	    | Some email -> email::acc
	    | None -> acc)
	[] (System.read_lines
	  ~filter:(Strings.substring_exists "Author: ")
	  "git log") in

    List.iter
      (fun commiter' ->
	let commiter = resolve_synonyms commiter' in
	if Hashtbl.mem table commiter then
	  let (cur,loc) = Hashtbl.find table commiter in
	  Hashtbl.replace table commiter ((succ cur), loc)
	else
	  Hashtbl.add table commiter (1, (search_loc commiter')))
      commiters
  in
  ignore(Component.with_component_dir
    ~strict:false component search);
  list_of_table table

let composite specdir =
  List.map
    (fun component ->
      component, commiters component)
    (List.filter
      (fun c -> c.Types.pkg = None && c.Types.name <> "pack")
      (Composite.load (Filename.concat specdir "composite")))

let html_quoting s =
  let b = Buffer.create 32 in
  String.iter (function
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    |  c  -> 
	 if Char.code c <= 127 then
	   Buffer.add_char b c) s;
  Buffer.contents b

let usergraph specdir =
  let dotfile = sprintf "%s.dot" (make_id "usergraph" specdir) in
  let pngfile = sprintf "%s.png" (make_id "usergraph" specdir) in

  let tree = Packtree.create ~default_branch:(Some (Specdir.branch specdir)) specdir in
  let depends =
    list_of_deptree (map_deptree fst tree) in
  
  let ch = open_out dotfile in
  let out = output_string ch in
  
  out "digraph g {\n";
  (*out "forcelabels = true;\n";*)
  out "graph [ rankdir = \"LR\" ];\n";
  out "node  [ fontname = \"Arial\", fontsize = \"10\" ];\n"; (* , //shape = record  *)
  out "edge  [ labelfontname = \"Arial\", labelfontsize = \"10\" ];\n";  (* , //style = dashed *)
  
  let string_of_vr = function
    | Some (ver,rev_opt) ->
	sprintf "%s%s" ver (match rev_opt with Some rev -> sprintf "-%d" rev  | None -> "")
    | None -> "any"
  in
  
  (* Nodes *)
  List.iter
    (fun n ->
      let label = Buffer.create 32 in
      let add = Buffer.add_string label in
      let colspan = if use_loc then 3 else 2 in
      add (sprintf "<table><tr><td colspan=\"%d\">" colspan);
      add (Specdir.pkgname n);
      add "</td></tr>";
      let data = composite n in
      List.iter
	(fun (component,commiters) ->
	  add (sprintf "<tr><td colspan=\"%d\" bgcolor=\"white\">%s</td></tr>" colspan (Component.infostring component));
	  List.iter
	    (fun (k,commits,loc_opt) ->
	      if use_loc then
		let loc =
		  match loc_opt with
		    | None -> "unknown"
		    | Some loc -> string_of_int loc in
		add (sprintf "<tr><td>%s</td><td>%s</td><td>%d</td></tr>" (html_quoting k) loc commits)
	      else
		add (sprintf "<tr><td>%s</td><td>%d</td></tr>" (html_quoting k) commits))
	    commiters)
	data;
      add "</table>";
      out (sprintf "\"%s\" [label=< %s >,shape=box,style=\"rounded,filled\",fillcolor=\"#77CC77\"]\n"
	(Specdir.pkgname n) (Buffer.contents label)))
    depends;

  let rec write_links parent = function
    | Dep_val (e, tree) ->
	let (en,evr_opt) = e in
	(match parent with
	  | Some p ->
	      let (pn,pvr_opt) = p in
	      if Version.have_revision evr_opt then
		out (sprintf "\"%s\" -> \"%s\" [label=\"%s\", color=\"black\"]\n"
		  (Specdir.pkgname pn)
		  (Specdir.pkgname en)
		  (string_of_vr evr_opt))
	      else
		out (sprintf "\"%s\" -> \"%s\" [label=\"%s\", color=\"black\", style=\"dashed\"]\n"
		  (Specdir.pkgname pn)
		  (Specdir.pkgname en)
		  (string_of_vr evr_opt));
	      write_links (Some e) tree
	  | None ->
	      write_links (Some e) tree)
    | Dep_list l ->
	List.iter (write_links parent) l
  in

  write_links None tree;
  
  out "}\n";
  close_out ch;
  make_image dotfile pngfile;
  view_image pngfile

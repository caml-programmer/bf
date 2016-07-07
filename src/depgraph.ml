open Output
open Spectype
open Component
open Hashtbl_ext
open String_ext
  
exception Pkg_already_regestered of pkg_name
exception Pkg_not_exists of pkg_name
exception Dependency_on_different_versions of (pkg_name * string * string)
exception Dependency_not_found of (pkg_name * pkg_name)
						
type spectable = (pkg_name, spec) Hashtbl.t
type depgraph = pkg_name Digraph.t ref * spectable

let create () : depgraph = ref (Digraph.create ()), Hashtbl.create 0
let digraph depgraph = fst depgraph
let spectable depgraph = snd depgraph

let has_pkg depgraph pkgname =
  Digraph.has_vertex !(digraph depgraph) pkgname

let get_spec depgraph pkgname =
  Hashtbl.find (spectable depgraph) pkgname

let reg_pkg depgraph spec =
  if has_pkg depgraph spec.pkgname then
    raise (Pkg_already_regestered spec.pkgname)
  else
    begin
      Output.msg "reg_pkg" "very-high" ("New pkg: "^spec.pkgname);
      let digraph = digraph depgraph in
      let spectable = spectable depgraph in
      digraph := Digraph.insert_vertex !digraph spec.pkgname;
      Hashtbl.add spectable spec.pkgname spec
    end

let upd_pkg depgraph spec =
  if has_pkg depgraph spec.pkgname then
    Hashtbl.replace (spectable depgraph) spec.pkgname spec
  else
    raise (Pkg_not_exists spec.pkgname)

let upd_pkg_if_rev_less depgraph spec =
  if has_pkg depgraph spec.pkgname then
    begin
      let current_spec = get_spec depgraph spec.pkgname in
      if current_spec.version <> spec.version then
	raise (Dependency_on_different_versions (spec.pkgname, current_spec.version, spec.version));
      if spec.revision < current_spec.revision then
	upd_pkg depgraph spec
    end
  else
    raise (Pkg_not_exists spec.pkgname)
	  
let has_circular_dep depgraph =
  Digraph.has_circular_path !(digraph depgraph)

let find_circular_dep depgraph =
  let path = Digraph.find_circular_path !(digraph depgraph) in
  string_of_string_list ~separator:" -> " path

let reg_dep depgraph head_pkg tail_pkg =
  let err = Output.err "reg_dep" in
  let msg = Output.msg "reg_dep" in
  let digraph = digraph depgraph in
  msg "very-high" ("New edge: "^head_pkg^" -> "^tail_pkg);
  digraph := Digraph.insert_edge !digraph head_pkg tail_pkg;
  if has_circular_dep depgraph then
    err ("Circular dependencies have occured: " ^ (find_circular_dep depgraph))

(* Загрузка графа зависимостей определённой ревизии *)
let of_pkg_with_rev ?(use_builddeps=false) pkgname version revision : depgraph =
  let func = "Depgraph.of_pkg_with_rev" in
  let msg = Output.msg func in

  (* Для загрузки актуальной информации о зависимостях пакета, надо
  прежде сделать checkout на тег, соответствующий состоянию пакета.
  Это нужно для того, чтобы исключить влияние возможных девелоперских
  коммитов, произошедших между установками тегов зависимого и базового
  пакетов. *)
  let specload pkg ver rev =
    if Pkg.is_local pkg then
      let tag = Tag.make pkg ver rev in
      msg "high" ("Checkout to tag "^tag);
      Pack.checkout tag;
      Spectype.newload pkg ver
    else
      Spectype.system_pkg_spec pkg ver in
  
  (* Создаём граф зависимостей, который мы будем императивно заполнять *)
  let depgraph = create () in
  (* Прописываем алиасы основных функций, которые будут работать только с этим графом *)
  let has_pkg = has_pkg depgraph in
  let reg_pkg = reg_pkg depgraph in
  let reg_dep = reg_dep depgraph in

  (* функция заполнения графа *)
  let rec fill_graph ?(prevpkg="") (pkg, ver, rev) =
    if has_pkg pkg then
      reg_dep prevpkg pkg
    else
      let spec = specload pkg ver rev in
      let depends = if use_builddeps then spec.builddeps else spec.depends in
      reg_pkg spec;
      (if prevpkg <> "" then reg_dep prevpkg pkg);
      let deps =
	List.map 
	  (function
	    | (deppkg, Some (_, depver), _) ->
		begin
		  match Pkg.is_local deppkg with
		    | false -> (deppkg, depver, 0)
		    | true ->
			let deprev = 
			  Specdir.revision_by_pkgver deppkg depver in
			(deppkg, depver, deprev)
		end
	    | _ -> assert false)
	  depends in
      List.iter (fill_graph ~prevpkg:pkg) deps in
  
  (* Основное тело функции *)
  fill_graph (pkgname, version, revision);
  depgraph

let of_pkg_only ?(use_builddeps=false) pkgname version =
  let func = "Depgraph.of_pkg_only" in
  let msg = Output.msg func in

  msg "always" ("test package: "^pkgname);
  msg "always" ("use_builddeps: "^(string_of_bool use_builddeps));
  
  let specload pkg ver =
    if Pkg.is_local pkg then
      Spectype.newload pkg ver
    else
      Spectype.system_pkg_spec pkg ver in

  let depgraph = create () in

  let has_pkg = has_pkg depgraph in
  let reg_pkg = reg_pkg depgraph in
  let reg_dep = reg_dep depgraph in

  let rec fill_graph ?(prevpkg="") (pkg, ver) =
    if has_pkg pkg then
      reg_dep prevpkg pkg
    else
      let spec = specload pkg ver in
      let depends = if use_builddeps then spec.builddeps else spec.depends in
      reg_pkg spec;
      (if prevpkg <> "" then reg_dep prevpkg pkg);
      let deps =
	List.map 
	  (function
	    | (deppkg, Some (_, depver), _) -> (deppkg, depver)
	    | _ -> assert false)
	  depends in
      List.iter (fill_graph ~prevpkg:pkg) deps in

  fill_graph (pkgname, version);
  depgraph

let of_pkg ?(use_builddeps=false) package version revision_opt =
  match revision_opt with
  | None -> of_pkg_only ~use_builddeps package version
  | Some revision -> of_pkg_with_rev ~use_builddeps package version revision

let string_of_deptree ?(use_builddeps=false) ?(limit_depth=1000000) (depgraph:depgraph) =
  let space depth = String.make (2*depth) ' ' in
  let graph = !(digraph depgraph) in
  let root = Digraph.find_root graph in
  
  let rec print_dep depth ?(prevpkg="") (pkg,deps) =
    if depth = limit_depth then ""
    else
      string_of_string_list
	(List.filter
	   not_string_empty
	   begin
	     ((space depth) ^ pkg ^ (if prevpkg <> "" then
				       let spec = get_spec depgraph prevpkg in
				       match Spectype.find_dependency ~use_builddeps spec pkg with
				       | None -> ""
				       | Some (op,ver) ->
					  let spec = get_spec depgraph pkg in
					  let rev = spec.revision in
					  let rev = if rev = 0 then ""
						    else "-"^(string_of_int rev) in
					  " "^(Spectype.string_of_pkg_op op)^" "^ver^rev
				     else
				       let spec = get_spec depgraph pkg in
				       "/"^(Spectype.release_of_spec spec)
				    ))
	     :: (List.map (fun dep -> print_dep ~prevpkg:pkg (succ depth)
						(Digraph.get_vertex graph dep))
			  deps)
	   end) in
  print_dep 0 (Digraph.get_vertex graph root)

let hashtbl_keys table =
  Hashtbl.fold (fun key _ acc -> key :: acc) table []
		
	    
let string_of_deplist ?(limit_depth=1000000) (depgraph:depgraph) =
  let spectable = spectable depgraph in
  let pkgnames = hashtbl_keys spectable in
  Output.string_of_string_list
    (List.map (fun pkgname ->
	       let spec = Hashtbl.find spectable pkgname in
	       let version = spec.version in
	       pkgname^"-"^version)
	      pkgnames)
		 
		 
let string_of_component_list (depgraph:depgraph) =
  let spectable = spectable depgraph in
  let pkgnames = hashtbl_keys spectable in
  Output.string_of_string_list
    (List.flatten (List.map (fun pkgname ->
			     let spec = Hashtbl.find spectable pkgname in
			     List.map (fun component ->
				       component.name ^
					 " ("^(string_of_label component.label)^")")
				      spec.components)
			    pkgnames))

let get_dep ?(use_builddeps=false) (depgraph:depgraph) head tail =
  Output.msg "get-dep" "very-high" (head^" -> "^tail);
  let spec = get_spec depgraph head in
  find_dependency ~use_builddeps spec tail

let draw ?(use_builddeps=false) ?(local_only=false) (depgraph:depgraph) file =
  let ch = if (file = "stdout") || (file = "-")
	   then stdout
	   else open_out file in
  let print = output_string ch in
  let print_endline str = print (str^"\n") in

  print_endline "digraph g {";
  (*print_endline "forcelabels = true;";*)
  print_endline "graph [ rankdir = \"LR\" ];";
  print_endline "node  [ fontname = \"Arial\", fontsize = \"10\" ];"; (* , //shape = record  *)
  print_endline "edge  [ labelfontname = \"Arial\", labelfontsize = \"10\" ];";  (* , //style = dashed *)

  let graph = !(digraph depgraph) in
  
  (* вершины *)
  List.iter
    (fun pkg ->
     if Pkg.is_local pkg then
       let color = "#77CC77" in
       print_endline ("\""^pkg^"\"\n"^"[shape=box,style=\"rounded,filled\",fillcolor=\""^color^"\"]");
       if Pkg.is_devel pkg then
	 let pkg_dev = pkg in
	 let pkg = Pkg.chop_devel_suffix pkg in
	 print_endline ("subgraph cluster_"^(String.unprintable_to_underline pkg)^" {");
	 print_endline ("style=filled;color=lightgrey;");
	 print_endline ("\""^pkg^"\"");
	 print_endline ("\""^pkg_dev^"\"");
	 print_endline "}"
     else
       if not local_only then
	 let color = "#FFFFFF" in
	 print_endline ("\""^pkg^"\"\n"^"[shape=box,style=\"rounded,filled\",fillcolor=\""^color^"\"]"))
    (Digraph.vertices graph);

  (* рёбра *)
  List.iter
    (fun (head, tail) ->
     let dep_opver = get_dep ~use_builddeps depgraph head tail in
     let label = 
       match dep_opver with
       | Some (op, ver) -> let opstr = string_of_op op in opstr^" "^ver
       | None -> " " in
     let non_strict = match dep_opver with
       | Some (op, ver) when ((op <> Pkg_eq) && (op <> Pkg_last)) -> ", style=\"dashed\""
       | _ -> "" in
     if local_only then
       (if (Pkg.is_local tail) && (Pkg.is_local head) then
	  print_endline ("\""^head^"\" -> \""^tail^"\" [label=\""^label^"\", color=\"black\""^non_strict^"]"))
     else
       print_endline ("\""^head^"\" -> \""^tail^"\" [label=\""^label^"\", color=\"black\""^non_strict^"]"))
    (Digraph.edges graph);

  print_endline "}";
  close_out ch;

  (* всякая ерунда, типа вывести на экран картинку *)
  let pngfile = (file^".png") in
  Graph.make_image file pngfile;
  try Graph.view_image pngfile
  with _ -> Output.warn "draw" "Can't start graphical viewer"

let union (depgraphs:depgraph list)= match depgraphs with
  | [] -> create ()
  | depgraph :: [] -> depgraph
  | depgraphs ->
     let depgraph = create () in
     (digraph depgraph) := Digraph.union (List.map (fun dg -> !(digraph dg)) depgraphs);
     Hashtbl.union_to (List.map spectable depgraphs) (spectable depgraph);
     depgraph

let filter (depgraphs:depgraph list) =
  ()
    
       
let subtree_buildgraph package version =
  let depgraph = of_pkg package version None in
  let depgraphs = Hashtbl.fold (fun pkgname spec acc ->
				(of_pkg ~use_builddeps:true pkgname spec.version None)::acc)
			       (spectable depgraph) [] in
  union depgraphs
  
			       

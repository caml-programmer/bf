(* Граф зависимостей -- производная от нагруженного орграфа
 * Сокращения:
 * dg - depgraph
 * 
 *)

open Spectype
open Component
open Ext
       
exception Pkg_already_regestered of pkg_name
exception Pkg_not_exists of pkg_name
exception Dependency_on_different_versions of (pkg_name * string * string)
exception Dependency_not_found of (pkg_name * pkg_name)
       
type spectable = (pkg_name, spec) Hashtbl.t
type depgraph =
  {
    graph: (pkg_name, pkg_opver) Ldigraph.t;
    spectable: spectable
  }

let create () =
  {
    graph = Ldigraph.create ();
    spectable = Hashtbl.create 0;
  }
		  
let graph dg = dg.graph
let spectable dg = dg.spectable

let pkgs dg = Ldigraph.vertices (graph dg)
let deps dg = Ldigraph.edges (graph dg)
let depinfo dg dep =
  try Some (Ldigraph.weight (graph dg) dep)
  with Not_found -> None
		      
let has_pkg dg pkg = Ldigraph.has_vertex (graph dg) pkg
let has_dep dg dep = Ldigraph.has_edge (graph dg) dep
let has_depinfo dg dep = Ldigraph.has_weight (graph dg) dep

let reg_pkg dg spec =
  let pkg = spec.pkgname in
  let msg = Output.msg "reg_pkg" in
  if has_pkg dg pkg then
    raise (Pkg_already_regestered pkg)
  else
    begin
      msg "very-high" ("New pkg: "^pkg);
      Ldigraph.insert_vertex (graph dg) pkg;
      Hashtbl.add (spectable dg) pkg spec
    end

let reg_dep dg (hpkg,tpkg) opver_opt =
  let msg = Output.msg "reg_dep" in
  let dep = (hpkg,tpkg) in
  match opver_opt with
  | Some opver ->
     msg "very-high" ("New dep: "^hpkg^" -> "^tpkg^" ("^(string_of_opver opver)^")");
     Ldigraph.insert_edge (graph dg) dep;
     Ldigraph.set_weight (graph dg) dep opver
  | None ->
     msg "very-high" ("New dep: "^hpkg^" -> "^tpkg);
     Ldigraph.insert_edge (graph dg) dep

let unreg_dep dg dep =
  Ldigraph.del_edge (graph dg) dep

let unreg_pkg dg pkg =
  Ldigraph.del_vertex (graph dg) pkg;
  Hashtbl.remove (spectable dg) pkg

let pkg_spec dg pkg = Hashtbl.find (spectable dg) pkg
let pkg_revdeps dg pkg = Ldigraph.in_edges (graph dg) pkg
let count_pkg_revdeps dg pkg = Ldigraph.count_in_edges (graph dg) pkg
let pkg_deps dg pkg = Ldigraph.out_edges (graph dg) pkg
let count_pkg_deps dg pkg = Ldigraph.count_out_edges (graph dg) pkg
let roots dg = Ldigraph.roots (graph dg)
let leaves dg = Ldigraph.leaves (graph dg)
let dep_head = Ldigraph.edge_head
let dep_tail = Ldigraph.edge_tail






(*		 
let upd_pkg dg spec =
  let pkg = spec.pkgname in
  if has_pkg dg pkg then
    Hashtbl.replace (spectable dg) pkg spec
  else raise (Pkg_not_exists pkg)
 *)
		 
let has_circular_deps dg =
  Ldigraph.has_circular_path (graph dg)

let find_circular_deps dg =
  Output.string_of_string_list ~separator:" -> "
			       (Ldigraph.find_circular_path (graph dg))

(* выбирает версию в соответствии с зависимостью *)
let choose_version pkg opver_opt =
  let err = Output.err "choose_version" in
  match opver_opt with
  | None -> ""
  | Some opver ->
     let op = fst opver in
     let ver = snd opver in
     if not (Pkg.is_local pkg) then ver
     else
       let pkg = Pkg.chop_devel_suffix pkg in
       match op with
       | Pkg_last | Pkg_eq | Pkg_le -> ver
       | Pkg_gt -> let maxver = Specdir.find_max_version pkg in
		   if Version.greater maxver ver then maxver
		   else err ("Can't find version for package '"^pkg^"' > "^ver)
       | Pkg_ge -> let maxver = Specdir.find_max_version pkg in
		   if not (Version.less ver maxver) then maxver
		   else err ("Can't find version for package '"^pkg^"' >= "^ver)
       | Pkg_lt -> let versions = Specdir.pkg_versions pkg in
		   let versions = List.filter (fun ver_avail -> Version.less ver_avail ver)
					      versions in
		   if (List.length versions) = 0 then
		     err ("Can't find version for package '"^pkg^"' < "^ver);
		   List.hd (List.rev (List.sort Version.cmp versions))

let of_pkg_ver ?(use_builddeps=false) pkg ver =
  (* загружает спек версии пакета в соответствии с зависимостью*)
  let specload pkg opver_opt =
    let load = if Pkg.is_local pkg
	       then Spectype.newload
	       else Spectype.system_pkg_spec in
    let ver = choose_version pkg opver_opt in
    load pkg ver in

  let dg = create () in
  let has_pkg = has_pkg dg in
  let has_dep = has_dep dg in
  let reg_pkg = reg_pkg dg in
  let reg_dep = reg_dep dg in

  let rec fill_graph spec =
    let dependencies = if use_builddeps then spec.builddeps else spec.depends in
    let hpkg = spec.pkgname in
    List.iter (fun (tpkg, opver_opt, _) ->
	       let spec = (specload tpkg opver_opt) in
	       if not (has_pkg tpkg) then reg_pkg spec;
	       let dep = (hpkg,tpkg) in
	       if not (has_dep dep) then reg_dep (hpkg,tpkg) opver_opt;
	       fill_graph spec)
	      dependencies in

  let spec = specload pkg (Some (Pkg_eq, ver)) in
  reg_pkg spec;
  fill_graph spec;
  dg
    
let of_pkg ?(use_builddeps=false) ?revision pkg ver =
  match revision with
  | None -> of_pkg_ver ~use_builddeps pkg ver
  | Some rev -> failwith "of_pkg: can't work with revision now"

let draw ?(local_only=false) dg =
  let file = "graph.dot" in
  let ch = open_out file in
  let print = output_string ch in
  let print_endline str = print (str^"\n") in

  print_endline "digraph g {";
  (*print_endline "forcelabels = true;";*)
  print_endline "graph [ rankdir = \"LR\" ];";
  print_endline "node  [ fontname = \"Arial\", fontsize = \"10\" ];"; (* , //shape = record  *)
  print_endline "edge  [ labelfontname = \"Arial\", labelfontsize = \"10\" ];";  (* , //style = dashed *)

  (* вершины (пакеты) *)
  List.iter
    (fun pkg ->
     let spec = pkg_spec dg pkg in
     let color = if spec.local then "#77CC77" else "#FFFFFF" in
     if spec.local || (not local_only) then
       begin
	 print_endline ("\""^pkg^"\"\n"^"[shape=box,style=\"rounded,filled\",fillcolor=\""^color^"\"]");
	 if Pkg.is_devel pkg then
	   let pkg_dev = pkg in
	   let pkg = Pkg.chop_devel_suffix pkg in
	   print_endline ("subgraph cluster_"^(String.unprintable_to_underline pkg)^" {");
	   print_endline ("style=filled;color=lightgrey;");
	   print_endline ("\""^pkg^"\"");
	   print_endline ("\""^pkg_dev^"\"");
	   print_endline "}"
       end)
    (pkgs dg);

  (* рёбра (зависимости) *)
  List.iter
    (fun (hpkg,tpkg) ->
     let hspec = pkg_spec dg hpkg in
     let tspec = pkg_spec dg tpkg in
     let opver_opt = depinfo dg (hpkg,tpkg) in
     let label = match opver_opt with
       | Some opver -> string_of_opver opver
       | _ -> "" in
     let non_strict = match opver_opt with
       | Some (op, ver) when ((*(op <> Pkg_eq) && *)(op <> Pkg_last)) -> ", style=\"dashed\""
       | _ -> "" in
     if (hspec.local && tspec.local) || (not local_only) then
       print_endline ("\""^hpkg^"\" -> \""^tpkg^"\" [label=\""^label^"\", color=\"black\""^non_strict^"]"))
    (deps dg);

  print_endline "}";
  close_out ch;

  (* всякая ерунда, типа вывести на экран картинку *)
  let pngfile = (file^".png") in
  Graph.make_image file pngfile;
  try Graph.view_image pngfile
  with _ -> Output.warn "draw" "Can't start graphical viewer"
       
let union (depgraphs:depgraph list) = match depgraphs with
  | [] -> create ()
  | dgs ->
     {
       graph = Ldigraph.union (List.map graph dgs);
       spectable = Hashtbl.union (List.map spectable dgs);
     }

let subtree_buildgraph pkg ver =
  let dg = of_pkg pkg ver in
  (* удаляем из графа все зависимости, кроме last *)
  List.iter (fun dep ->
	     match depinfo dg dep with
	     | Some (Pkg_last, _) -> ()
	     | _ -> unreg_dep dg dep)
	    (deps dg);

  (* строим графы сборочных зависимостей для всех пакетов dg *)
  let pkgvers = List.map (fun pkg -> (pkg, (pkg_spec dg pkg).version))
			 (pkgs dg) in
  let dgs = List.map (fun (pkg,ver) ->
		      of_pkg ~use_builddeps:true pkg ver)
		     pkgvers in

  (* объединяем исходный граф с остальными сборочными *)
  union (dg :: dgs)

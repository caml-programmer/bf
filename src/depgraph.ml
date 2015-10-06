open Output
open Spectype

exception Pkg_already_regestered of pkg_name
exception Pkg_not_exists of pkg_name
exception Dependency_on_different_versions of (pkg_name * string * string)
       
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
    let digraph = digraph depgraph in
    let spectable = spectable depgraph in
    digraph := Digraph.insert_vertex !digraph spec.pkgname;
    Hashtbl.add spectable spec.pkgname spec

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
  let digraph = digraph depgraph in
  digraph := Digraph.insert_edge !digraph head_pkg tail_pkg;
  if has_circular_dep depgraph then
    err "reg_dep" ("Circular dependencies have occured: " ^ (find_circular_dep depgraph))

(* Загрузка графа зависимостей *)
let of_pkg pkgname version revision : depgraph =
  let func = "Depgraph.of_pkg" in
  let err = Output.err func in
  let warn = Output.warn func in
  let msg = Output.msg func in

  (* Для загрузки актуальной информации о зависимостях пакета, надо
  прежде сделать checkout на тег, соответствующий состоянию пакета.
  Это нужно для того, чтобы исключить влияние возможных девелоперских
  коммитов, произошедших между установками тегов зависимого и базового
  пакетов. *)
  let specload pkg ver rev =
    if Package.is_local pkg then
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
  let get_spec = get_spec depgraph in
  let reg_pkg = reg_pkg depgraph in
  let upd_pkg = upd_pkg_if_rev_less depgraph in
  let reg_dep = reg_dep depgraph in

  (* функция заполнения графа *)
  let rec fill_graph ?(prevpkg="") (pkg, ver, rev) =
    if has_pkg pkg then
      reg_dep prevpkg pkg
    else
      let spec = specload pkg ver rev in
      reg_pkg spec;
      (if prevpkg <> "" then reg_dep prevpkg pkg);
      let deps =
	List.map (fun dep ->
		  let (deppkg, Some (op, depver), _) = dep in
		  match Package.is_local deppkg with
		  | false -> (deppkg, depver, 0)
		  | true ->
		     let deprev = Specdir.revision_by_pkgver deppkg depver in
		     (deppkg, depver, deprev))
		 spec.depends in
      List.iter (fill_graph ~prevpkg:pkg) deps in
		  
  (* Основное тело функции *)
  fill_graph (pkgname, version, revision);
  depgraph

let string_of_deptree ?(limit_depth=1000000) (depgraph:depgraph) =
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
				       match Spectype.find_dependency spec pkg with
				       | None -> ""
				       | Some (op,ver) -> " "^(Spectype.string_of_pkg_op op)^" "^ver
				     else
				       let spec = get_spec depgraph pkg in
				       "/"^(Spectype.release_of_spec spec)
				    ))
	     :: (List.map (fun dep -> print_dep ~prevpkg:pkg (succ depth)
						(Digraph.get_vertex graph dep))
			  deps)
	   end) in
  print_dep 0 (Digraph.get_vertex graph root)

	    

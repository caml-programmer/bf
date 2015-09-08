open Types
open Logger
open Printf

let non_empty_iter f = function
    []   -> log_error "don't know what to do"
  | list -> List.iter f list

let non_empty_map f = function
    []   -> log_error "don't know what to do"
  | list -> List.map f list

let non_empty_list = function
    []   -> log_error "don't know what to do"
  | list -> list

let rec max_value f cur = function
  | [] -> cur
  | hd::tl ->
      max_value f (max (f hd) cur) tl

let with_tag tag components =
  match tag with
    | None -> components
    | Some tag_name ->
	List.map
	  (fun component ->
	    {
	      name = component.name; 
	      label = Tag tag_name; 
	      pkg = component.pkg; 
	      rules = component.rules;
	      nopack = component.nopack;
	      forkmode = component.forkmode;
	    })
	  components

(* возвращает только те компоненты, которые собираются в рамках данного пакета *)
let only_local components =
  List.filter
    (fun c -> c.pkg = None) components

let only_external components =
  List.filter
    (fun c -> c.pkg <> None) components

let as_current l =
  List.map (fun c -> 
    { 
      name = c.name; 
      label = Current; 
      pkg = c.pkg; 
      rules = c.rules;
      nopack = c.nopack;
      forkmode = c.forkmode;
    }) l

let tag_ready ~tag components =
  List.for_all
    (fun component ->
      let res = ref false in
      ignore
	(Component.with_component_dir ~strict:false component
	  (fun () -> res := List.mem tag (Git.git_tag_list ())));
      !res)
    (as_current (only_local components))

let status components =
  let max_component_length =
    max_value (fun n -> String.length n.name) 0 components in
  let max_label_length =
    max_value (fun n -> String.length (string_of_label n.label)) 0 components in
  non_empty_iter
    (fun component -> 
      Component.status ~max_component_length ~max_label_length component)
    components

let make_tag tag components =
  non_empty_iter (Component.tag tag) components

let make_diff tag_a tag_b components =
  non_empty_iter (Component.diff tag_a tag_b) components

let prepare components =
  non_empty_iter Component.prepare components

let forward components =
  non_empty_iter Component.forward components

let build components =
  non_empty_iter Component.build components

let rebuild components =
  non_empty_iter Component.rebuild components

let install components =
  List.fold_left
    (fun acc component ->
      let reinstalled =
	Component.install component in
      if reinstalled then
	component::acc
      else
	acc)
    [] (non_empty_list components)

let reinstall components =
  non_empty_iter Component.reinstall components

let update components =
  List.exists (fun x -> x) (List.map Component.update components)

let make_review since components =
  ignore(update components);

  let chunks = ref [] in
  let add s = chunks:=s::!chunks in
  non_empty_iter
    (fun component -> 
      let tag = string_of_label component.label in
      List.iter add (Component.changelog ~diff:false ~since:(Some since) tag tag component)) components;
  add "\n------------------ DIFF -------------------\n";
  non_empty_iter
    (fun component ->
      let tag = string_of_label component.label in
      List.iter add (Component.changelog ~diff:true ~since:(Some since) tag tag component))
    components;

  Notify.send_message
    ~subject:(Printf.sprintf "bf@review (%s)" since)
    ~contents:(List.rev !chunks)
    (Params.get_param "smtp-notify-email")

let make_changelog ?(interactive=false) ?(compact=false) ?(branch=None) tag_a tag_b components =
  let chunks = ref [] in
  let add s = chunks:=s::!chunks in
  non_empty_iter
    (fun component -> 
      List.iter add (Component.changelog ~branch ~diff:false tag_a tag_b component)) components;
  if not compact then
    begin
      add "\n------------------ DIFF -------------------\n";
      non_empty_iter
	(fun component ->
	  List.iter add (Component.changelog ~branch ~diff:true tag_a tag_b component))
	components;
    end;
  if interactive then
    begin
      printf "bf@changelog %s -> %s\n" tag_a tag_b;
      List.iter print_endline (List.rev !chunks)
    end
  else
    Notify.send_message
      ~subject:(Printf.sprintf "bf@changelog %s -> %s" tag_a tag_b)
      ~contents:(List.rev !chunks)
      (Params.get_param "smtp-notify-email")

let changelog components tag_a tag_b =
  log_message ("=> changelog-components " ^ tag_a ^ ":" ^ tag_b);
  let first_rev =
    match Tag.parse tag_a with
      | Some (_,_,0) -> true
      | _ -> false in
  if not first_rev then   
    begin
      try
	make_changelog tag_a tag_b (only_local components)
      with exn ->
	log_message (sprintf "=> changelog-failed by %s\n" (Printexc.to_string exn))
    end

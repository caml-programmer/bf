open Ocs_types
open Printf
open Logger

let load file =
  let composite = ref None in
  Env.prepare ();
  Scheme.eval_file file;
  Scheme.eval_code (fun v -> composite := Some v) "(composite)";
  match !composite with
    | None -> log_error "composite handler is not called"
    | Some v -> 
	(Scheme.map Scheme.component_of_sval v)

let write file components =
  let ch = open_out file in
  let out = output_string ch in
  let first = ref true in
  let write c =
    if not !first then
      out "\n  ";
    out "(";
    out c.Types.name;    
    (match c.Types.label with
      | Types.Current -> ()
      | Types.Branch s ->
	  out " (branch \"";
	  out s;
	  out "\")";
      | Types.Tag s ->
	  out " (tag \"";
	  out s;
	  out "\")");
    (match c.Types.pkg with
      | None -> ()
      | Some s ->
	  out " (package \"";
	  out s;
	  out "\")");
    if c.Types.nopack then
	  out " (nopack)";
    (match c.Types.rules with
      | None -> ()
      | Some s ->
	  out " (rules \"";
	  out s;
	  out "\")");   
    out ")";
    first := false;
  in
  out "(define (composite)\n'(";
  List.iter write components;
  out "))\n";
  close_out ch

let components ?(replace_composite=None) composite =
  let replace =
    match replace_composite with
      | None -> []
      | Some x -> load x in
  List.map
    (fun c ->
      match List.filter 
	(fun r -> 
	  r.Types.name  = c.Types.name &&
	  r.Types.rules = c.Types.rules)
	replace with
	  | r::_ ->
	      printf "replace %s.%s.%s -> %s.%s.%s\n%!"
		c.Types.name (Types.string_of_label c.Types.label) (Types.string_of_rules c.Types.rules)
		r.Types.name (Types.string_of_label r.Types.label) (Types.string_of_rules r.Types.rules);
	      r
	  | [] -> c)
    (load composite)


let prepare ?tag composite =
  log_message ("=> prepare-composite " ^ composite);
  Components.prepare (Components.with_tag tag (components composite))

let update ?tag composite =
  log_message ("=> update-composite " ^ composite);
  Components.update (Components.with_tag tag (components composite))

let forward ?tag composite =
  log_message ("=> forward-composite " ^ composite);
  Components.forward (Components.with_tag tag (components composite))
  
let build ?tag composite =
  log_message ("=> build-composite " ^ composite);
  Components.build (Components.with_tag tag (components composite))

let rebuild ?tag composite =
  log_message ("=> rebuild-composite " ^ composite);
  Components.rebuild (Components.with_tag tag (components composite))

let install ?tag composite =
  log_message ("=> install-composite " ^ composite);
  Components.install (Components.with_tag tag (components composite))

let reinstall ?tag composite =
  log_message ("=> reinstall-composite " ^ composite);
  Components.reinstall (Components.with_tag tag (components composite))

let status ?tag composite =
  log_message ("=> status-composite " ^ composite);
  Components.status (Components.with_tag tag (components composite))

let tag composite tag =
  log_message ("=> tag-composite " ^ composite ^ " " ^ tag);
  Components.make_tag tag 
    (Components.only_local
      (components composite))

let review composite since =
  log_message ("=> review-composite " ^ composite ^ " " ^ since);
  Components.make_review since (components composite)

let diff composite tag_a tag_b =
  log_message ("=> diff-composite " ^ composite ^ " " ^ tag_a ^ ":" ^ tag_b);
  Components.make_diff tag_a tag_b 
    (Components.only_local (components composite))

let changelog ?(interactive=false) ?(compact=false) composite tag_a tag_b =
  log_message ("=> changelog-composite " ^ composite ^ " " ^ tag_a ^ ":" ^ tag_b);
  let branch =
    Some (Filename.basename (Filename.dirname composite)) in  
  Components.make_changelog ~interactive ~compact ~branch tag_a tag_b
    (Components.only_local (components composite))









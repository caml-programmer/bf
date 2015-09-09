exception Bad_forkmode of string

type label = Tag of string | Branch of string | Current

type forkmode =
  | Tagging
  | Branching
  | Inherit

type component = {
  name  : string;
  label : label;
  pkg : string option;
  rules : string option;
  nopack: bool;
  forkmode : forkmode;
}

let forkmode_of_string = function
  | "do-branch" | "branch" | "branching" -> Branching
  | "fixtag" | "tag" | "tagging"         -> Tagging
  | "inherit"                            -> Inherit
  | x -> raise (Bad_forkmode x)

let string_of_forkmode = function
  | Tagging -> "tagging"
  | Branching -> "branching"
  | Inherit -> "inherit"
	       
let string_of_label = function
  | Tag s -> s
  | Branch s -> s
  | Current -> ""

let string_of_label_type = function
  | Tag _    -> "tag"
  | Branch _ -> "branch"
  | Current  -> "current"

let string_of_rules = function
  | None -> ""
  | Some r -> r

type content_status =
  | Tree_prepared               (* nothing to do *)
  | Tree_changed of string list (* checkout -f && clean -d *)

type worktree_status =
  | Tree_not_exists            (* do remove, clone and checkout -f *)
  | Tree_exists_with_given_key of content_status
  | Tree_exists_with_other_key of string (* do checkout -f and clean -d *)

type tag_status =
  | Tag_already_exists
  | Tag_created
  | Tag_creation_problem

type version = string
type revision = int

let string_of_string_option = function
  | Some (x:string) -> x
  | None -> ""

open String
open Printf
	      
let string_of_component comp = 
  concat "\n"
		[
		  (sprintf "NAME: %s" comp.name);
		  (sprintf "LABEL: %s %s" (string_of_label_type comp.label)
			   (string_of_label comp.label));
		  (sprintf "PKG: %s" (string_of_string_option comp.pkg));
		  (sprintf "RULES: %s" (string_of_string_option comp.rules));
		  (sprintf "NOPACK: %B" comp.nopack);
		  (sprintf "FORKMODE: %s" (string_of_forkmode comp.forkmode));
		  ""
		]


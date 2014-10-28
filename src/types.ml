type label = Tag of string | Branch of string | Current

type component = {
  name  : string;
  label : label;
  pkg : string option;
  rules : string option;
  nopack: bool;
}

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

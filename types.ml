type label = Tag of string | Branch of string | Current

type component = {
  name  : string;
  label : label;
}

let string_of_label = function
  | Tag s -> s
  | Branch s -> s
  | Current -> ""

let string_of_label_type = function
  | Tag _    -> "tag"
  | Branch _ -> "branch"
  | Current  -> "current"

type content_status =
  | Tree_prepared              (* nothing to do *)
  | Tree_changed               (* checkout -f && clean -d *)

type worktree_status =
  | Tree_not_exists            (* do remove, clone and checkout -f *)
  | Tree_exists_with_given_key of content_status
  | Tree_exists_with_other_key (* do checkout -f and clean -d *)

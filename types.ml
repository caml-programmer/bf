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

(** old

type key_status =
  | Exists     (* need checkout *)
  | Not_exists (* need reclone in composite-mode *)
  | Be_set     (* prepared *)

type worktree_status =
  | Exists     (* checkout or recreate *)
  | Not_exists (* clone, checkout -> create *)
  | Be_set     (* check tag && branch if needed *)

type branch_status =
  | Branch_is_obsolete
  | Branch_is_freshen
  | Branch_is_according

type key_status =
  | Tag_exists_at_local_repos
  | Tag_not_exists_at_local_repos
  | Branch_exists_at_local_repos of branch_status
  | Branch_not_exists_at_local_repos

type worktree_status =
  | Not_exists
  | Exists_with_given_key_but_different of branch_status option
  | Be_set
  | Exists_with_different_key of key_status
*)

type content_status =
  | Tree_prepared              (* nothing to do *)
  | Tree_changed               (* checkout -f && clean -d *)

type worktree_status =
  | Tree_not_exists            (* do remove, clone and checkout -f *)
  | Tree_exists_with_given_key of content_status
  | Tree_exists_with_other_key (* do checkout -f and clean -d *)

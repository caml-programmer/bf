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

type key_status =
  | Exists     (* need checkout *)
  | Not_exists (* need reclone in composite-mode *)
  | Be_set     (* prepared *)

type worktree_status =
  | Exists     (* checkout or recreate *)
  | Not_exists (* clone, checkout -> create *)
  | Be_set     (* check tag && branch if needed *)

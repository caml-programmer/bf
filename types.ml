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

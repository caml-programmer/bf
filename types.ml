type label = Tag of string | Branch of string | Current

type component = {
  name  : string;
  label : label;
}

let string_of_label = function
  | Tag s -> s
  | Branch s -> s
  | Current -> ""

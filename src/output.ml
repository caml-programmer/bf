(* данный модуль реализует функционал, относящийся к формированию
сообщений, которые подлежат выводу на экран *)

let current_log_level () =
  (Params.get_param "log-level")

let int_of_log_level loglevel =
  match loglevel with
  | "always" -> 0
  | "low" -> 1
  | "high" -> 2
  | "debug" | "all" -> 999
  | "never" -> 1000000
  | _ -> failwith "Unknown log-level"

let log_level_acceptable loglevel =
  let syslevel = int_of_log_level (current_log_level ()) in
  let funlevel = int_of_log_level loglevel in
  funlevel <= syslevel

let string_list_of_string string =
  Str.split (Str.regexp "\n") string

let string_of_string_list strings =
  String.concat "\n" strings

let prefix_textblock prefix string =
  let strings = string_list_of_string string in
  string_of_string_list (List.map (fun str -> prefix ^ str) strings)

let msg func loglevel msg =
  if log_level_acceptable loglevel then
    print_endline (prefix_textblock (func ^ ": ") msg)

let count_strings string =
  List.length (string_list_of_string string)
		  
(* строковые предикаты *)
		  
let equal_strings s1 s2 =
  0 = String.compare s1 s2

let not_equal_strings s1 s2 =
  not (equal_strings s1 s2)

let string_empty s =
  Str.string_match (Str.regexp "^[ \t]*$") s 0

let not_string_empty s =
  not (string_empty s)

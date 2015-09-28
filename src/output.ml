(* данный модуль реализует функционал, относящийся к формированию
сообщений, которые подлежат выводу на экран *)

let current_log_level () =
  (Params.get_param "log-level")

let int_of_log_level loglevel =
  match loglevel with
  | "always" | "error" -> 0
  | "low" -> 1
  | "high" -> 2
  | "debug" | "all" -> 999
  | "never" -> 1000000
  | _ -> failwith "Unknown log-level"

let log_level_acceptable loglevel =
  let syslevel = int_of_log_level (current_log_level ()) in
  let funlevel = int_of_log_level loglevel in
  funlevel <= syslevel

let string_list_of_string ?(separator="\n") string =
  Str.split (Str.regexp separator) string

let string_of_string_list ?(separator="\n") strings =
  String.concat separator strings

let prefix_textblock prefix string =
  let strings = string_list_of_string string in
  string_of_string_list (List.map (fun str -> prefix ^ str) strings)

let enumerate_string_list strings =
  let len = 1 + (List.length strings) in
  let add_num num str =
    let num_str = (string_of_int num) in
    let len_to_fill = len - (String.length num_str) in
    let prefix = num_str ^ (String.make len_to_fill ' ') in
    match (string_list_of_string str) with
    | first_line :: rest_lines ->
       string_of_string_list [(prefix^first_line);
			      (prefix_textblock (String.make len ' ')
						(string_of_string_list rest_lines))]
    | [] -> "" in
  let rec enum acc num strings =
    match strings with
    | str :: rest ->
       enum ((add_num num str) :: acc)
	    (succ num)
	    rest
    | [] -> string_of_string_list (List.rev acc) in
  enum [] 1 strings

let msg func loglevel msg =
  if log_level_acceptable loglevel then
    print_endline (prefix_textblock (func ^ ": ") msg)

let err func message =
  msg func "error" message;
  failwith message
		  
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

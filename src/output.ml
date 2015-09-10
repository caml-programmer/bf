
open Params
open Printf
open String

let current_log_level () =
  (get_param "log-level")

let int_of_log_level loglevel =
  match loglevel with
  | "low" -> 1
  | "high" -> 2
  | "debug" -> 999
  | "all" -> 999
  | _ -> failwith "Unknown log-level"

let log_level_acceptable loglevel =
  let syslevel = int_of_log_level (current_log_level ()) in
  let funlevel = int_of_log_level loglevel in
  funlevel <= syslevel

let msg func loglevel msg =
  if log_level_acceptable loglevel then
    print_endline (func ^ ": " ^ msg)

let string_list_of_string string =
  Str.split (Str.regexp "\n") string

let string_of_string_list strings =
  String.concat "\n" strings

let prefix_textblock prefix string =
  let strings = string_list_of_string string in
  string_of_string_list (List.map (fun str -> prefix ^ str) strings)

open Types
open Printf
open List
open String
open Output
       
type pkg_name = string
type pkg_desc = string
type pkg_ver = string
type pkg_op =
  | Pkg_le
  | Pkg_lt
  | Pkg_eq
  | Pkg_ge
  | Pkg_gt
  | Pkg_last

type platform_depend =
    pkg_name * (pkg_op * pkg_ver) option * pkg_desc option

type reject =
    string (* pcre pattern *)

type provide =
    string (* string symbol *)

type spec = {
  pkgname : pkg_name;
  depends : platform_depend list;
  provides : provide list;
  obsoletes : provide list;
  rejects : reject list;
  components : component list;
  pre_install : string option;
  pre_update : string option;
  pre_uninstall : string option;
  post_install : string option;
  params : (string,string) Hashtbl.t;
  hooks : string option;
}

let string_of_pkg_op = function
  | Pkg_le -> "<="
  | Pkg_lt -> "<"
  | Pkg_eq -> "="
  | Pkg_ge -> ">="
  | Pkg_gt -> ">"
  | Pkg_last -> "="

let string_of_op = function
  | Pkg_last -> "last"
  | v -> string_of_pkg_op v

exception Pack_branch_is_not_found of string

let string_of_platform_depend (pkg_name, dep, desc) =
  string_of_string_list
    [
      pkg_name;
      (match dep with
       | Some (op, ver) -> 
	  sprintf " %s %s" (string_of_pkg_op op) ver
       | None -> "");
      ""
    ]

let string_of_spec spec =
  string_of_string_list
    [
      (sprintf "PKGNAME: %s" spec.pkgname);
      "DEPENDS:";
      (prefix_textblock "  " (string_of_string_list
				(List.map string_of_platform_depend spec.depends)));
      "PRIVIDES:";
      (prefix_textblock "  " (string_of_string_list spec.provides));
      "OBSOLETES:";
      (prefix_textblock "  " (string_of_string_list spec.obsoletes));
      "REJECTS:";
      (prefix_textblock "  " (string_of_string_list spec.rejects));
      "COMPONENTS:";
      (prefix_textblock "  " (string_of_string_list
				(List.map string_of_component spec.components)));
      (sprintf "PRE_INSTALL: %s" (string_of_string_option spec.pre_install));
      (sprintf "PRE_UPDATE: %s" (string_of_string_option spec.pre_update));
      (sprintf "PRE_UNINSTALL: %s" (string_of_string_option spec.pre_uninstall));
      (sprintf "POST_INSTALL: %s" (string_of_string_option spec.post_install));
      "PARAMS: TODO";
      (sprintf "HOOKS: %s" (string_of_string_option spec.hooks));
    ]


open Ocamlbuild_plugin

let have_yojson =
  Sys.command "ocamlfind query yojson 2>/dev/null 2>&1" = 0

let pkg_opts =
  if have_yojson then
    [A "-package"; A "yojson"]
  else
    []
  
let define_handle = function
  | "YOJSON" as s -> S ([A "-ppopt"; A ((if have_yojson then "-D" else "-U") ^ s)] @ pkg_opts)
  | _        -> S []

let _ =
  dispatch
    (function
      | After_rules ->
	  (* Define options for files _tags handling *)
	  pflag ["ocaml";"compile" ] "define" define_handle;
	  pflag ["ocaml";"ocamldep"] "define" define_handle;

	  (* Add optional packages *)
	  flag ["ocaml"; "link"] & S pkg_opts
      | _ -> ())

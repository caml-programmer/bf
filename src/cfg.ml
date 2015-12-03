(* этот модуль расширяет Params, обеспечивая ему перезагрузку параметров из новых файлов *)
module Params = struct
    include Params
    open String_ext

    let global_config_file = "/etc/bf.conf"
    let config_file = Path.expand_globs "~/.bf2/config"

    let read_from_file filename =
      let rex = Re_perl.compile_pat "^([^\\s]+)\\s+(.*)\\s*$" in
      List.iter
	(fun s -> let res = Re.exec rex s in
		  let key = Re.get res 1 in
		  let value = Re.get res 2 in	
		  try set key value
		  with Unknown_parameter _ -> Printf.printf "ignore: %s\n%!" s)
	(List.filter String.not_empty (System.list_of_file filename))

    let load_cfg () =
      read_from_file global_config_file;
      read_from_file config_file

    let reload_cfg () =
      reread_params ();
      load_cfg ()
  end

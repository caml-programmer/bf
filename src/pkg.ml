open String_ext

let devel_suffix () = "-dev"
       
let is_devel pkg_name =
  let dev_suffix = devel_suffix () in
  Filename.check_suffix pkg_name dev_suffix

let chop_devel_suffix pkg_name =
  let dev_suffix = devel_suffix () in
  String.chop_suffix pkg_name dev_suffix
			
let is_local pkg_name =
  let pkg_name = chop_devel_suffix pkg_name in
  let pack_param = Params.get "pack" in
  let pkg_dir = Path.make [pack_param;pkg_name] in
  (Sys.file_exists pkg_dir) && (Sys.is_directory pkg_dir)

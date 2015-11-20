let is_local pkg_name =
  let dev_suffix = "-dev" in
  let pkg_name = if Filename.check_suffix pkg_name dev_suffix then
		   Filename.chop_suffix pkg_name dev_suffix
		 else pkg_name in
  let pkg_dir = Path.make ["pack";pkg_name] in
  (Sys.file_exists pkg_dir) && (Sys.is_directory pkg_dir)

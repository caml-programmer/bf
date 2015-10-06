
let checkout ?(loglevel="high") tag =
  System.with_dir (Params.get_param "pack")
		  (fun () -> (Git.checkout ~loglevel tag))

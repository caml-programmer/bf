open Printf

let full_require =
  Pcre.regexp "([^\\ ]+)\\s+([<>]?=)\\s+([^-]+)-(\\d+)\\."

let without_rev_require =
  Pcre.regexp "([^\\ ]+)\\s+([<>]?=)\\s+(.+)"

let without_ver_require =
  Pcre.regexp "(.+)"

exception Cannot_extract_revision of string
  
let extract ~userhost pkg_path =
  List.rev
    (List.fold_left
      (fun acc s ->
	try
	  let a = Pcre.extract ~rex:full_require s in
	  let pkg_name = a.(1) in
	  if Params.home_made_package pkg_name then
	    begin
	      let operand = a.(2) in
	      let ver = a.(3) in
	      let rev = 
		try int_of_string a.(4)	with _ ->
		  raise (Cannot_extract_revision pkg_path) in
	      (pkg_name,Some ver,Some rev, Some operand)::acc
	    end
	  else acc
	with Not_found ->
	  (try
	    let a = Pcre.extract ~rex:without_rev_require s in	    
	    let pkg_name = a.(1) in
	    if Params.home_made_package pkg_name then
	      begin
		let operand = a.(2) in
		let ver = a.(3) in
		(pkg_name,Some ver,None, Some operand)::acc
	      end
	    else acc
	  with Not_found ->
	    (try 
	      let a = Pcre.extract ~rex:without_ver_require s in
	      let pkg_name = a.(1) in
	      if Params.home_made_package pkg_name then
		(pkg_name,None,None, None)::acc
	      else acc
	    with Not_found -> acc))) []
      (System.read_lines
	~filter:Params.home_made_package
	(match userhost with
	  | Some auth ->
	      (sprintf "ssh %s rpm -qRp %s" auth pkg_path)
	  | None -> 
	      (sprintf "rpm -qRp %s" pkg_path))))

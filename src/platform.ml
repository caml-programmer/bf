open Logger
open Printf

type os =
  | Linux
  | SunOS

type pkg_engine =
  | Rpm_build
  | Pkg_trans
  | Deb_pkg

type platform =
  | Rhel3
  | Rhel4
  | Rhel5
  | Rhel6
  | Rhel7
  | Cent4
  | Cent5
  | Cent6
  | Cent7
  | Fedora10
  | Alt
  | Arch
  | Solaris8
  | Solaris9
  | Solaris10
  | Debian
  | Gentoo
  | Unknown_linux

type platform_mapping =
    (string * ((string * platform) list)) list

exception Permanent_error of string
exception Pkg_engine_not_found

let os_of_platform = function
  | Rhel3          -> Linux
  | Rhel4          -> Linux
  | Rhel5          -> Linux
  | Rhel6          -> Linux
  | Rhel7          -> Linux
  | Cent4          -> Linux
  | Cent5          -> Linux
  | Cent6          -> Linux
  | Cent7          -> Linux
  | Fedora10       -> Linux
  | Alt            -> Linux
  | Arch           -> Linux
  | Debian         -> Linux
  | Gentoo         -> Linux
  | Unknown_linux  -> Linux
  | Solaris8       -> SunOS
  | Solaris9       -> SunOS
  | Solaris10      -> SunOS    

let engine_of_platform = function
  | Rhel3     -> Rpm_build
  | Rhel4     -> Rpm_build
  | Rhel5     -> Rpm_build
  | Rhel6     -> Rpm_build
  | Rhel7     -> Rpm_build
  | Cent4     -> Rpm_build
  | Cent5     -> Rpm_build
  | Cent6     -> Rpm_build
  | Cent7     -> Rpm_build
  | Fedora10  -> Rpm_build
  | Alt       -> Rpm_build
  | Arch      -> Rpm_build
  | Solaris8  -> Pkg_trans
  | Solaris9  -> Pkg_trans
  | Solaris10 -> Pkg_trans
  | Debian    -> Deb_pkg
  | Gentoo | Unknown_linux
      -> raise Pkg_engine_not_found

let string_of_platform = function
  | Rhel3     -> "rhel3"
  | Rhel4     -> "rhel4"
  | Rhel5     -> "rhel5"
  | Rhel6     -> "rhel6"
  | Rhel7     -> "rhel7"
  | Cent4     -> "cent4"
  | Cent5     -> "cent5"
  | Cent6     -> "cent6"
  | Cent7     -> "cent7"
  | Fedora10  -> "f10"
  | Alt       -> "alt"
  | Arch      -> "arch"
  | Solaris8  -> "sol8"
  | Solaris9  -> "sol9"
  | Solaris10 -> "sol10"
  | Debian    -> "deb"
  | Gentoo    -> "gentoo"
  | Unknown_linux -> "linux"

let platform_of_string = function
  | "rhel3" -> Rhel3
  | "rhel4" -> Rhel4
  | "rhel5" -> Rhel5
  | "rhel6" -> Rhel6
  | "rhel7" -> Rhel7
  | "cent4" -> Cent4
  | "cent5" -> Cent5
  | "cent6" -> Cent6
  | "cent7" -> Cent7
  | "f10"   -> Fedora10
  | "alt"   -> Alt
  | "arch"  -> Arch
  | "sol8"  -> Solaris8
  | "sol9"  -> Solaris9
  | "sol10" -> Solaris10
  | "deb"   -> Debian
  | "gentoo" -> Gentoo
  | "linux" -> Unknown_linux
  |  s -> log_error (sprintf "Unsupported platform (%s)" s)

let os_of_string = function
  | "linux" -> Linux
  | "sunos" -> SunOS
  | s -> log_error (sprintf "Unsupported OS (%s)" s)

let string_of_os = function
  | Linux -> "linux"
  | SunOS -> "sunos"
  | _ -> Output.err "Platform.string_of_os" "Unknown OS"
		   
let os_as_string = System.uname

let os () =
  os_of_string (os_as_string ())

let linux_platform_mapping =
  [
    "/etc/redhat-release",
    [
      "^Red Hat Enterprise.*?release 5",Rhel5;
      "^Red Hat Enterprise.*?release 6",Rhel6;
      "^Red Hat Enterprise.*?release 7",Rhel7;
      "^CentOS.*?release 5",Rhel5;
      "^CentOS.*?release 6",Rhel6;
      "^CentOS.*?release 7",Rhel7;
      "^Fedora.*?release 10",Fedora10;
      "^ALT Linux",Alt
    ];
    "/etc/arch-release", ["^.*",Arch];
    "/etc/debian_version",["^.*",Debian];
    "/etc/gentoo-release",["^.*",Gentoo];
  ]

let rec select_platforms acc = function
  | [] -> acc
  | (file,mapping)::tl ->
      if Sys.file_exists file then
	begin
	  let s = System.read_file ~file in
	  let l = 
	    List.filter
	      (fun (pat,_) -> Pcre.pmatch ~rex:(Pcre.regexp pat) s) mapping in
	  (match l with
	    | (_,platform)::_ -> 
		select_platforms (acc @ [platform]) tl
	    | _ -> 
		select_platforms acc tl)
	end
      else select_platforms acc tl

let current () =
  match select_platforms [] linux_platform_mapping with
  | [] -> Unknown_linux
  | p::_ -> p
			    
let sunos_platfrom () =
  match System.uname ~flag:'r' () with
    | "5.8"  -> Solaris8
    | "5.9"  -> Solaris9
    | "5.10" -> Solaris10
    |  s     ->	log_error (sprintf "Unsupported SunOS (%s)" s)

let with_platform (f : os -> platform -> 'a) =
  let os = os () in
  match os with
    | Linux ->
	let platform =
	  (match select_platforms [] linux_platform_mapping with
	    | [] -> Unknown_linux
	    | p::_ -> p)
	in f os platform
    | SunOS ->
	f os (sunos_platfrom ())


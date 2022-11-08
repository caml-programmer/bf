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
  | Rhel
  | Alt
  | Solaris
  | Debian
  | Ubuntu
  | Astra
  | Arch
  | Gentoo
  | Unknown_linux

type platform_mapping =
    (string * ((string * platform) list)) list

exception Permanent_error of string
exception Pkg_engine_not_found

let os_of_platform = function
  | Rhel           -> Linux
  | Alt            -> Linux
  | Astra          -> Linux
  | Arch           -> Linux
  | Debian         -> Linux
  | Ubuntu         -> Linux
  | Gentoo         -> Linux
  | Unknown_linux  -> Linux
  | Solaris        -> SunOS

let engine_of_platform = function
  | Rhel      -> Rpm_build
  | Alt       -> Rpm_build
  | Arch      -> Rpm_build
  | Solaris   -> Pkg_trans
  | Debian    -> Deb_pkg
  | Ubuntu    -> Deb_pkg
  | Astra     -> Deb_pkg
  | Gentoo | Unknown_linux
      -> raise Pkg_engine_not_found

let string_of_platform = function
  | Rhel      -> "rhel"
  | Alt       -> "alt"
  | Arch      -> "arch"
  | Solaris   -> "sol"
  | Debian    -> "deb"
  | Ubuntu    -> "ubuntu"
  | Astra     -> "astra"
  | Gentoo    -> "gentoo"
  | Unknown_linux -> "linux"

let platform_of_string = function
  | "rhel" -> Rhel
  | "alt"   -> Alt
  | "arch"  -> Arch
  | "sol"   -> Solaris
  | "deb"   -> Debian
  | "astra" -> Astra
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

let os_as_string = System.uname

let os () =
  os_of_string (os_as_string ())

let linux_platform_mapping =
  [
    "/etc/altlinux-release", [".*", Alt];
    "/etc/redhat-release",
    [
      "^Red Hat Enterprise.*?release",Rhel;
    ];
    "/etc/arch-release", ["^.*",Arch];
    "/usr/lib/os-release", ["Ubuntu",Ubuntu];
    "/etc/astra_version", [".*", Astra];
    "/etc/debian_version", [".*",Debian];
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
    | "5.10" -> Solaris
    |  s     -> log_error (sprintf "Unsupported SunOS (%s)" s)

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

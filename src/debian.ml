open Printf

let fix_arch = function
  | "i686" -> "i386"
  | "i586" -> "i386"
  | "x86_64" -> "amd64"
  | s -> s

let fullname pkgname version revision arch =
  sprintf "%s_%s-%s_%s.deb" pkgname version revision arch

let codename () =
  let ch = open_in "/etc/os-release" in
  let result = ref "" in
  begin
    try
      while true do
        match Strings.split '=' (input_line ch) with
          "VERSION_CODENAME"::value::_ ->
           result := value
        | _ -> ()
      done
    with End_of_file ->  close_in ch;
  end; !result

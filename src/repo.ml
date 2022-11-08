open Platform
open Output

let rhel_release_pkg_url mirror_url =
  let (_,pkg,_) =
    Cmd.command ("curl -s "^mirror_url^"Packages/ | grep -Po 'redhat-release.*?\\.rpm' | head -1") in
  mirror_url^"Packages/"^pkg

let pkg_by_url pkgurl =
  Filename.basename pkgurl (* Suck a hack, isn't it? *)

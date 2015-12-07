open Cmdliner
open Cfg
open Params
open Chroot

(* Перезагрузка конфигурации описана в модуле Cfg *)
let () = reload_cfg ();;
(* Если используется данный бинарник, установить bf-version в 2 *)
Params.set "bf-version" "2";;

(* Определение основных секций *)
let bugs_secs = [
    `S "BUGS";
    `P "Send bug reports to freehck@freehck.ru";]
       
let copts_sect = "COMMON OPTIONS"

let help_secs = [
 `S copts_sect;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."]

let copts loglevel = loglevel

let copts_t =
  let docs = copts_sect in
  let verbosity =
    let doc = "Set verbosity level." in
    Arg.(value & flag & info ["v"; "verbose"] ~docs ~doc)
  in
  Term.(pure copts $ verbosity)

(* Перегрузка параметров *)

let params_table = Hashtbl.create 32

let reg_param_opt pname doc =
  let arg = Arg.(value & opt string "" & info [pname] ~docs:copts_sect ~doc) in
  Term.(pure Hashtbl.add $ pure params_table $ pure pname $ arg)
  
let apply_param_opts () =
  Hashtbl.iter (fun param value ->
		if value <> "" then
		  Params.set param value)
	       params_table

	       (*
let param_opts_term =
  let docs = copts_sect in
  let loglevel_param = "log-level" in
  let loglevel_arg =
    let doc = "Variates log-level option\nCould be: always, low, high, very[-]high, all" in
    Arg.(value & opt string "" & info [loglevel_param] ~docs ~doc) in
  let params = [(loglevel_param, loglevel_arg)] in
  Term.(pure apply_param_opts $ pure params)
      
let loglevel_opt =
  let docs = copts_sect in
  let doc = "Variates loglevel option\nCould be: always, low, high, very[-]high, all" in
  Arg.(value & opt string "" & info ["log-level"] ~docs ~doc);;

Term.(pure set_param_with_option $ pure "log-level" $ loglevel_opt)
		*)
	       
(* ДЕЙСТВИЯ *)
      
(* Действие по умолчанию -- вычисление терма документации к программе *)
let default_cmd =
  let doc = "Build Farm 2" in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info (Filename.basename Sys.argv.(0))
	    ~version:"2.0.0" ~sdocs:copts_sect ~doc ~man

let cmds = ref []
let regcmd cmd = cmds := cmd :: !cmds
let cmds () = !cmds

(* Выводит на экран все параметры *)
let params_cmd =
  let doc = "Print current state for all configuration parameters" in
  let man = help_secs in
  Term.(pure Params.print_params $ pure ()),
  Term.info "params" ~sdocs:copts_sect ~doc ~man;;
regcmd params_cmd

(* Вывести на экран дерево run-time зависимостей *)
let deptree_cmd =
  let doc = "Print run-time dependencies tree" in
  let man = help_secs in

  let pkgname =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PKGNAME") in
  let version =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"VERSION") in
  let revision_opt =
    Arg.(value & pos 2 (some int) None & info [] ~docv:"REVISION") in
  
  Term.(pure Test.depgraph $ pkgname $ version $ revision_opt),
  Term.info "deptree" ~doc ~man;;
regcmd deptree_cmd
	    
(* Вывести на экран дерево build зависимостей *)

let buildtree_cmd =
  let doc = "Print build dependencies tree" in
  let man = help_secs in
  
  let pkgname =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PKGNAME") in
  let version =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"VERSION") in
  let revision_opt =
    Arg.(value & pos 2 (some int) None & info [] ~docv:"REVISION") in

  Term.(pure Test.buildgraph $ pkgname $ version $ revision_opt),
  Term.info "buildtree" ~doc ~man;;
regcmd buildtree_cmd

(* Вывести на экран список пакетов, являющихся зависимостями данного *)
let deplist_cmd =
  let doc = "Print run-time dependency list" in
  let man = help_secs in

  let pkgname =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PKGNAME") in
  let version =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"VERSION") in

  Term.(pure Test.deplist $ pkgname $ version),
  Term.info "deplist" ~doc ~man;;
regcmd deplist_cmd


(* Вывести на экран список компонентов, входящих в пакеты из deplist_cmd *)
let depcomps_cmd =
  let doc = "Print components of run-time dependencies" in
  let man = help_secs in

  let pkgname =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PKGNAME") in
  let version =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"VERSION") in

  Term.(pure Test.depcomps $ pkgname $ version),
  Term.info "depcomps" ~doc ~man;;
regcmd depcomps_cmd
	   
(* buildpkg *)

let buildpkg_cmd =
  let doc = "Build package in chroot environment" in
  let man = help_secs in

  let pkgname =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PKGNAME") in
  let version =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"VERSION") in
  let platform =
    Arg.(required & pos 2 (some string) None & info [] ~docv:"PLATFORM") in

  Term.(pure (fun pkgname version platform ->
	      let platform = Platform.platform_of_string platform in
	      let os = Platform.os_of_platform platform in
	      let pkgspec = Spectype.newload ~os ~platform pkgname version in
	      Chroot.buildpkg ~os ~platform pkgspec)
	$ pkgname $ version $ platform),
  Term.info "buildpkg" ~doc ~man;;
regcmd buildpkg_cmd

(* packpkg *)

let packpkg_cmd =
  let doc = "Pack package from a chroot environment where it was already built in" in
  let man = help_secs in

  let chroot_name =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CHROOT_NAME") in
  let pkgname =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"PKGNAME") in
  let version =
    Arg.(required & pos 2 (some string) None & info [] ~docv:"VERSION") in
  let platform =
    Arg.(required & pos 3 (some string) None & info [] ~docv:"PLATFORM") in

  Term.(pure (fun chroot_name pkgname version platform ->
	      let platform = Platform.platform_of_string platform in
	      let os = Platform.os_of_platform platform in
	      let pkgspec = Spectype.newload ~os ~platform pkgname version in
	      Chroot.pack ~os ~platform chroot_name pkgspec)
	$ chroot_name $ pkgname $ version $ platform),
  Term.info "packpkg" ~doc ~man;;
regcmd packpkg_cmd

(* chroot-shell *)
let chroot_shell_cmd =
  let doc = "Open a shell in the chroot environment" in
  let man = help_secs in

  let chroot_name =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CHROOT_NAME") in
  let platform =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"PLATFORM") in
  let shell =
    Arg.(value & pos 2 string "/bin/bash" & info [] ~docv:"SHELL") in

  Term.(pure (fun chroot_name platform shell ->
	      let platform = Platform.platform_of_string platform in
	      let chroot = Chroot.load_chroot_cfg chroot_name platform in
	      Chroot.chroot_shell chroot.path shell)
	$ chroot_name $ platform $ shell),
  Term.info "chroot-shell" ~doc ~man;;
regcmd chroot_shell_cmd

(* непосредственно старт программы *)
let () = match Term.eval_choice default_cmd (cmds ()) with
  | `Error _ -> exit 1
  | _ -> exit 0

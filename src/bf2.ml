open Cmdliner
open Cfg
open Params
open Chroot

(* Перезагрузка конфигурации описана в модуле Cfg *)
let () = reload_cfg ();;
(* Если используется данный бинарник, установить bf-version в 2 *)
Params.set "bf-version" "2";;
(* Портаемся в root-dir, чтобы сделать вид, что мы оттуда запущены *)
let root_dir = Params.get "root-dir";;
Sys.chdir root_dir;;
  
(* Определение основных секций *)
let bugs_secs = [
    `S "BUGS";
    `P "Send bug reports to freehck@freehck.ru";]
       
let copts_sect = "OPTIONS"

let help_secs = [
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `P "Use `$(mname) [--help] to see all the commands.";
  ]

let copts table = table
  
let copts_t =
  let docs = copts_sect in
  let copts_table = Hashtbl.create 0 in
  
  let verbosity_name = "verbose" in
  let verbosity_arg =
    let doc = "Set verbosity level." in
    Arg.(value & flag & info ["v"; verbosity_name] ~docs ~doc) in
  Hashtbl.add copts_table verbosity_name verbosity_arg;

  Term.(pure copts $ pure copts_table)

(* Перегрузка параметров *)

type copts =
  {
    topdir: string option;
    devdir: string option;
  }

let copts topdir devdir =
  {
    topdir = topdir;
    devdir = devdir
  }
    
let str_of_opt converter = function None -> "" | Some v -> (converter v)
let str_of_opt_str = str_of_opt (fun s -> s)

let parse_copts copts =
  let getval = str_of_opt_str in
  let setparam pname value =
    if value <> None then Params.set pname (getval value) in
  setparam "top-dir" copts.topdir;
  setparam "dev-dir" copts.devdir;
  ()

let copts_t =
  let docs = copts_sect in
  let topdir =
    let doc = "Set top-dir for current run" in
    Arg.(value & opt (some string) None & info ["top-dir"] ~docv:"DIR" ~docs ~doc) in
  let devdir =
    let doc = "Set dev-dir for current run" in
    Arg.(value & opt (some string) None & info ["dev-dir"] ~docv:"DIR" ~docs ~doc) in
  Term.(pure copts $ topdir $ devdir)
      
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
  Term.(pure (fun copts -> parse_copts copts; Params.print_params ()) $ copts_t),
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

(* Нарисовать граф run-time зависимостей *)
let draw_depgraph_cmd =
  let doc = "Draw an image of run-time dependency graph" in
  let man = help_secs in

  let pkgname =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PKGNAME") in
  let version =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"VERSION") in

  Term.(pure Test.draw_depgraph $ pkgname $ version),
  Term.info "draw-depgraph" ~doc ~man;;
regcmd draw_depgraph_cmd

(* Нарисовать граф build-зависимостей *)
let draw_buildgraph_cmd =
  let doc = "Draw an image of build-dependency graph" in
  let man = help_secs in

  let pkgname =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PKGNAME") in
  let version =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"VERSION") in

  Term.(pure Test.draw_buildgraph $ pkgname $ version),
  Term.info "draw-buildgraph" ~doc ~man;;
regcmd draw_buildgraph_cmd
       
(* draw-subtree-buildgraph *)
let draw_subtree_buildgraph =
  let doc = "Draw a subtree buildgraph" in
  let man = help_secs in

  let pkgname =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PKGNAME") in
  let version =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"VERSION") in

  Term.(pure Test.draw_subtree_buildgraph $ pkgname $ version),
  Term.info "draw-subtree-buildgraph" ~doc ~man;;
regcmd draw_subtree_buildgraph

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

  Term.(pure (fun copts pkgname version platform ->
	      let platform = Platform.platform_of_string platform in
	      let os = Platform.os_of_platform platform in
	      let pkgspec = Spectype.newload ~os ~platform pkgname version in
	      parse_copts copts;
	      Chroot.buildpkg ~os ~platform pkgspec)
	$ copts_t $ pkgname $ version $ platform),
  Term.info "buildpkg" ~doc ~man;;
regcmd buildpkg_cmd

(* build-subtree *)
let build_subtree_cmd =
  let doc = "Build package and all its run-time dependencies.\n"^
	      "The graph of these-dependencies could be viewed by\n"^
		"draw-subtree-buildgraph subcommand" in
  let man = help_secs in

  let pkgname =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PKGNAME") in
  let version =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"VERSION") in
  let platform =
    Arg.(required & pos 2 (some string) None & info [] ~docv:"PLATFORM") in
  let threads =
    let doc = "Build with NUM threads" in
    Arg.(value & opt int 6 & info ["j"; "threads"] ~docv:"NUM" ~doc) in

  Term.(pure (fun copts threads pkgname version platform ->
	      let platform = Platform.platform_of_string platform in
	      let os = Platform.os_of_platform platform in
	      let pkgspec = Spectype.newload ~os ~platform pkgname version in
	      parse_copts copts;
	      Chroot.build_subtree ~threads ~os ~platform pkgspec)
	$ copts_t $ threads $ pkgname $ version $ platform),
  Term.info "build-subtree" ~doc ~man;;
regcmd build_subtree_cmd

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

let build_component_cmd =
  let doc = "Build component in a chroot environment" in
  let man = help_secs in
  
  let chroot_name =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CHROOT_NAME") in
  let component_name =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"COMPONENT") in
  let rules_opt =
    Arg.(value & pos 2 (some string) None & info [] ~docv:"RULES") in
  
  Term.(pure (fun copts chroot_name component_name rules_opt ->
	      parse_copts copts;
	      Chroot.build_component chroot_name component_name rules_opt)
	$ copts_t $ chroot_name $ component_name $ rules_opt),
  Term.info "build-component" ~doc ~man;;
regcmd build_component_cmd
       
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

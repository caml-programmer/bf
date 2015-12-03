open Cmdliner
open Cfg
open Params
    
let copts_sect = "COMMON OPTIONS"
let help_secs = [
 `S copts_sect;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
 `S "BUGS"; `P "Send bug reports to freehck@freehck.ru";]

let copts loglevel = loglevel
let copts_t =
  let docs = copts_sect in
  let verbosity =
    let doc = "Set verbosity level." in
    Arg.(value & flag & info ["v"; "verbose"] ~docs ~doc)
  in
  Term.(pure copts $ verbosity)

(* тестовая функция *)
let test text =
  print_endline ("TEST FUNCTION: "^text)
       
let test_cmd =
  let text =
    let doc = "print $(docv)" in
    Arg.(value & pos 0 string "Teeext?" & info [] ~docv:"TEXT" ~doc)
  in
  let doc = "just prints a massage on the screen" in
  let man = [
      `S "DESCRIPTION";
      `P "Print some message and immediately quits";] in
  Term.(pure test $ text),
  Term.info "test" ~doc ~man

(* Действие по умолчанию -- вычисление терма документации к программе *)
let default_cmd =
  let doc = "Build Farm 2" in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info (Filename.basename Sys.argv.(0))
	    ~version:"1.6.1" ~sdocs:copts_sect ~doc ~man

let cmds = [test_cmd;]

(* непосредственно старт программы *)

let () = reload_cfg () (* Перезагрузка конфигурации описана в модуле Cfg *)
let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0

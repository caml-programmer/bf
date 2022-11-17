let debug =
  try
    ignore(Sys.getenv "DEBUG"); true
  with Not_found -> false

let load () =
  if debug then print_string "load ac-configure...";
  Scheme.eval_code (fun _ -> if debug then print_endline "ok") "
(define-syntax ac-configure
  (syntax-rules ()
    ((_ (e1 e2) ...) (ml-ac-configure `((e1 ,e2) ...)))
    ((_ (e1 e2))     (ml-ac-configure `((e1 ,e2))))
    ((_ e1 e2)       (ml-ac-configure `((e1 ,e2))))
    ((_ (e))         (ml-ac-configure `((e ()))))
    ((_ e)           (ml-ac-configure `((e ()))))
    ((_)             (ml-ac-configure `()))))";

  if debug then print_string "load make...";
  Scheme.eval_code (fun _ -> if debug then print_endline "ok") "
(define-syntax make
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-make `((e ()) (e1 ,e2) ...)))
    ((_ (e1 e2) ...)     (ml-make `((e1 ,e2) ...)))
    ((_ (e1 e2))         (ml-make `((e1 ,e2))))
    ((_ e1 e2)           (ml-make `((e1 ,e2))))
    ((_ (e))             (ml-make `((e ()))))
    ((_ e)               (ml-make `((e ()))))
    ((_)                 (ml-make `()))))";

  if debug then print_string "load export...";
  Scheme.eval_code (fun _ -> if debug then print_endline "ok") "
(define-syntax export
  (syntax-rules ()
    ((_ (e1 e2) ...) (ml-export `((e1 ,e2) ...)))
    ((_ (e1 e2))     (ml-export `((e1 ,e2))))
    ((_ e1 e2)       (ml-export `((e1 ,e2))))
    ((_ (e))         (ml-export `((e ()))))
    ((_ e)           (ml-export `((e ()))))
    ((_)             (ml-export `()))))";

  if debug then print_string "load update-make-params...";
  Scheme.eval_code (fun _ -> if debug then print_endline "ok") "
(define-syntax update-make-params
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-update-make-params `((e ()) (e1 ,e2) ...)))
    ((_  e  (e1 e2) ...) (ml-update-make-params `((e ()) (e1 ,e2) ...)))))";

  if debug then print_string "load user lib.scm...";
  let dir = Params.get_param "plugins-dir" in
  let plugdir =
    let start = Params.get_param "start-dir" in
    if System.is_regular (Filename.concat dir "lib.scm") then
      dir
    else
      Filename.concat start dir
  in
  if not (System.is_regular (Filename.concat plugdir "lib.scm")) then
    (if debug then print_endline "skip")
  else
    begin
      System.with_extension "scm"
	Scheme.eval_file
	(System.with_prefix plugdir (System.list_of_directory plugdir));
      (if debug then print_endline "ok");
    end;
 if debug then Printf.printf "plugins-dir: %s\n" plugdir

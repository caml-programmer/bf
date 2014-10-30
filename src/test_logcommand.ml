open Test
open Printf

let logcommand1 () =
  try
    ignore(Logger.log_command "ls" ["-la";"/"]);
    true
  with exn ->
    printf "logcommand1: %s\n" (Printexc.to_string exn);
    false

let run () =
  test "LogCommand1" logcommand1

open Printf

let command ?(env=Unix.environment()) ?(ignore_errors=false) ?(filter=(fun _ -> true)) command =
  let (pout,pin,perr) = Unix.open_process_full command env in
  let rec read acc ch =
    try
      let s = input_line ch in
      if filter s
      then read (s::acc) ch
      else read acc ch
    with End_of_file -> acc in
  let outputs = String.concat "\n" (read [] pout) in
  let errors = String.concat "\n" (read [] perr) in
  let status = Unix.close_process_full (pout,pin,perr) in
  match status with
  | Unix.WEXITED st -> if ignore_errors || st = 0
		       then (st, outputs, errors)
		       else failwith (sprintf "Command '%s' exited with non-nil status: %d" command st)
  | Unix.WSIGNALED signal -> failwith (sprintf "Command '%s' was killed by signal: %d" command signal)
  | Unix.WSTOPPED signal -> failwith (sprintf "Command '%s' was stopped by signal: %d" command signal)

let command_log ?(loglevel="always") ?(env=Unix.environment()) ?(ignore_errors=false) ?(filter=(fun _ -> true)) cmd_str =
  let msg str = Output.msg "command" loglevel str in
  msg ("Run \"" ^ cmd_str ^ "\"");
  let (st,outputs,errors) = command ~env ~ignore_errors:true ~filter cmd_str in
  msg outputs;
  msg (Output.prefix_textblock "STDERR: " errors);
  msg ("DONE: exit code "^(string_of_int st));
  if ignore_errors || st = 0
  then (st,outputs,errors)
  else failwith (sprintf "Command '%s' exited with non-nil status: %d" cmd_str st)

let cpu_number () =
  let channel = open_in "/sys/devices/system/cpu/present" in
  let present_cores = input_line channel in
  close_in channel;
  let list_cores = Str.split (Str.regexp "-") present_cores in
  succ (int_of_string (List.nth list_cores (pred (List.length list_cores))))

open Printf

(* честно утащил из ds-config *)
let read_descriptors fdlist =
  let buffer_map = List.map (fun fd -> Unix.set_nonblock fd;
				       (fd,(Buffer.create 32)))
			    fdlist in
  let data = String.create 1024 in
  let append fd len =
    let chunk =
      String.sub data 0 len in
    let buf =
      List.assoc fd buffer_map in
    if Buffer.length buf + len < Sys.max_string_length then
      Buffer.add_string buf chunk 
  in
  let rec read l =
    if l <> [] then
      let (rl,_,_) = Unix.select l [] [] 0.1 in
      let ready =
	List.fold_left
	  (fun acc fd ->
	    let n = Unix.read fd data 0 1024 in
	    if n = 0 then
	      begin
		fd::acc
	      end
	    else
	      begin
		append fd n;
		acc
	      end) [] rl
      in
      read (List.filter (fun fd -> not (List.mem fd ready)) l)
    else () in
  read fdlist;
  List.map (fun (_,b) -> Buffer.contents b) buffer_map

let command ?(env=Unix.environment()) ?(ignore_errors=false) command =
  let msg str = Output.msg "command" "always" str in
  msg ("Run \"" ^ command ^ "\"");
  let (pout,pin,perr) = Unix.open_process_full command env in
  let pout_fd = Unix.descr_of_in_channel pout in
  let perr_fd = Unix.descr_of_in_channel perr in
  let [outputs; errors] = read_descriptors [pout_fd; perr_fd] in
  let status = Unix.close_process_full (pout,pin,perr) in
  match status with
  | Unix.WEXITED st -> if ignore_errors || st = 0
		       then (st, outputs, errors)
		       else failwith (sprintf "Command '%s' exited with non-nil status: %d" command st)
  | Unix.WSIGNALED signal -> failwith (sprintf "Command '%s' was killed by signal: %d" command signal)
  | Unix.WSTOPPED signal -> failwith (sprintf "Command '%s' was stopped by signal: %d" command signal)

let command_log ?(loglevel="always") ?(env=Unix.environment()) ?(ignore_errors=false) cmd_str =
  let msg str = Output.msg "command" loglevel str in
  let (st,outputs,errors) = command ~env ~ignore_errors:true cmd_str in
  msg (Output.prefix_textblock "STDOUT: " outputs);
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

let i_am_root =
  Unix.getuid () = 0

let sudo_cmd_exists =
  let sudo = Params.get "sudo-path" in
  Sys.file_exists sudo

let su_cmd_exists =
  let su = Params.get "su-path" in
  Sys.file_exists su

let prefer_sudo =
  match Params.get "prefer-sudo" with
  | "false" | "#f" | "nil" | "no" | "f" -> false
  | _ -> true

exception Nor_su_nor_sudo

let root_command ?loglevel cmd =
  let command = match loglevel with
    | None -> command
    | Some loglevel -> command_log ~loglevel in
  if i_am_root then
    command cmd
  else
    match prefer_sudo,sudo_cmd_exists,su_cmd_exists with
    | true,true,true  | _,true,false ->
       let sudo_command = "sudo "^cmd in
       command sudo_command
    | false,true,true | _,false,true ->
       let su_command = "su -c \""^cmd^"\"" in
       command su_command
    | _,false,false -> raise Nor_su_nor_sudo
			     
let choose_command ?(as_root=false) ?loglevel () =
  match as_root with
  | true -> root_command ?loglevel
  | false -> (match loglevel with
	      | None -> command ?env:None ?ignore_errors:None
	      | _ -> command_log ?env:None ?ignore_errors:None ?loglevel)

			     
let mkdir ?(as_root=false) ?loglevel dir =
  let command = choose_command ~as_root ?loglevel () in
  ignore (command ("mkdir "^dir))

let rmdir ?(as_root=false) ?loglevel ?(recursive=false) dir =
  let command = choose_command ~as_root ?loglevel () in
  let recursive_flag = if recursive then "-rf " else "" in
  ignore (command ("rm "^recursive_flag^dir))
    
let mkdir_if_not_exists ?(as_root=false) dir =
  if not (Sys.file_exists dir)
  then mkdir ~as_root dir

let rmdir_if_exists ?(as_root=false) ?(recursive=false) dir =
  if (Sys.file_exists dir) && (Sys.is_directory dir)
  then rmdir ~as_root ~recursive dir
						   
let move ?(as_root=false) ?loglevel src dst =
  let command = choose_command ~as_root ?loglevel () in
  ignore (command ("mv "^src^" "^dst))

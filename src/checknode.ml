(* Модуль для проверки ресурсов сборочного стенда: checknode.ml *)

open Printf

let df () =
  let ch =
    Unix.open_process_in "df -P" in

  let chop s =
    let l = String.length s in
    String.sub s 0 (pred l) in

  let space_split =
    Str.split (Str.regexp " +") in

  let rec read acc =
    try
      read ((input_line ch)::acc)
    with End_of_file ->
      close_in ch;
      List.rev acc in
  
  let mountpoints =
    match read [] with
      | _::tl -> tl
      | _     -> [] in
  
  try
    List.map
      (fun mountpoint ->
	let x = space_split mountpoint in
	let device = List.nth x 0 in
	let point = List.nth x 5 in
	let usage = int_of_string (chop (List.nth x 4)) in
	(device,point,usage))
      mountpoints
  with exn ->
    failwith (sprintf "unknown 'df -P' format")

let checkdisk size =
  List.fold_left 
    (fun acc (device,point,usage) ->
      if usage > 90 then
	((sprintf "[checkdisk]: Device(%s), Mountpoint(%s), CriticalSize(%d%%)" device point usage)::acc)
      else acc)
    [] (df ())
    
let generate_events generators =
  List.fold_left
    (fun acc generator ->
      acc @ (generator ()))
    [] generators

let start ~smtp_server ~smtp_port emails =
  let messages =
    generate_events
      [checkdisk] in
  if messages <> [] then
    begin
      List.iter
	(Notify.send_message
	  ~smtp_server
	  ~smtp_port
	  ~subject:"BF checknode"
	  ~from_name:"BF"
	  ~from_mail:(sprintf "bf@%s" (Unix.gethostname ()))
	  ~contents:messages)
	emails
    end


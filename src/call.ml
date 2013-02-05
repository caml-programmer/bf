open Printf

type program = string
type arguments = string list
type call_result =
  | Exited of int * string * string
  | Signaled of int * string * string
  | Stopped of int * string * string

exception Failed of (program * arguments * call_result)

let read_descriptors l =
  let buffer_map =
    List.map
      (fun fd ->
	Unix.set_nonblock fd;
	fd,Buffer.create 32) l in
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
		Unix.close fd;
		fd::acc
	      end
	    else
	      begin
		append fd n;
		acc
	      end) [] rl
      in
      read (List.filter (fun fd -> not (List.mem fd ready)) l)
    else ()
  in read l;
  List.map
    (fun (_,b) -> Buffer.contents b) buffer_map

let read program args =
  let (rn,wn) = Unix.pipe () in
  let (re,we) = Unix.pipe () in
  match Unix.fork () with
    | 0 ->  (* child *)
	Unix.dup2 wn Unix.stdout;
	Unix.dup2 we Unix.stderr;
	let arguments =
	  Array.of_list (program::args) in
	if Sys.file_exists program then
	  Unix.execv  program arguments
	else
	  Unix.execvp program arguments
    | pid ->
	Unix.close wn; Unix.close we;
	match read_descriptors [rn;re] with
	    [ output; errors ] ->
	      let (_,ps) = Unix.waitpid [] pid in
	      if ps = Unix.WEXITED 0 then
		(output,errors)
	      else
		(match ps with
		  | Unix.WEXITED rc ->
		      raise (Failed (program,args,(Exited (rc,output,errors))))
		  | Unix.WSIGNALED n ->
		      raise (Failed (program,args,(Signaled (n,output,errors))))
		  | Unix.WSTOPPED n ->
		      raise (Failed (program,args,(Stopped (n,output,errors)))))
	  | _ -> assert false

let run program args =
  ignore(read program args)

let string_of_result = function
  | Exited (rc,output,error) -> 
      sprintf "(exited (code %d) (output %s) (error %s))" rc output error
  | Signaled (n,output,error) ->
      sprintf "(signaled (code %d) (output %s) (error %s))" n output error
  | Stopped (n,output,error) ->
      sprintf "(stopped (code %d) (output %s) (error %s))" n output error

let string_of_error (program,args,result) =
  sprintf "failed call: %s %s -> %s"
    program (String.concat " " args) (string_of_result result)


module List = struct
    include List

    let remove_elt e l =
      let rec go l acc = match l with
	| [] -> List.rev acc
	| x::xs when e = x -> go xs acc
	| x::xs -> go xs (x::acc)
      in go l []

    let remove_duplicates l =
      let rec go l acc = match l with
	| [] -> List.rev acc
	| x :: xs -> go (remove_elt x xs) (x::acc)
      in go l []

    let get n l =
      if n <= 0 then failwith "get";
      let rec rget r n l =
	if n = 0 then
	  rev r
	else
	  match l with
	  | h::t -> rget (h::r) (pred n) t
	  | [] -> rget r 0 l in
      rget [] n l

  end

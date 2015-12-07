module String = struct
    include String

    let empty s =
      Str.string_match (Str.regexp "^[ \t]*$") s 0

    let not_empty s =
      not (empty s)

    let chop_suffix str suff =
      if Filename.check_suffix str suff then
	Filename.chop_suffix str suff
      else str

  end

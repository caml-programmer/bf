module String = struct
    include String

    let empty s =
      Str.string_match (Str.regexp "^[ \t]*$") s 0

    let not_empty s =
      not (empty s)
  end

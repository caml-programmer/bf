open Test

let regexp1 () =
  Pcre.pmatch ~rex:(Pcre.regexp ("^" ^ "KEY" ^ "\\s*=.*?$")) "KEY = VALUE"
let regexp2 () =
  not (Pcre.pmatch ~rex:(Pcre.regexp ("^" ^ "KEY" ^ "\\s*=.*?$")) "MESSAGE TEXT")
let regexp3 () =
  (Re.Group.get (Re.exec (Re.Perl.compile_pat "(\\d+)") "X = 45") 1 = "45")
let regexp4 () =
  Pcre.split ~rex:(Re.compile (Re.rep1 Re.space))
    "a b  c    d" = ["a";"b";"c";"d"]
let regexp5 () =
  Commands.replace_param "A" (Some "/c") "SRC_DIR=/jet\nA=/b" = "SRC_DIR=/jet\nA=/c"

let run () =
  test "Regexp 1" regexp1;
  test "Regexp 2" regexp2;
  test "Regexp 3" regexp3;
  test "Regexp 4" regexp4;
  test "Regexp 5" regexp5

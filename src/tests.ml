open Printf

let test name call =
  printf "%s: %b\n" name (call ())

let regexp1 () =
  Pcre.pmatch ~rex:(Pcre.regexp ("^" ^ "KEY" ^ "\\s*=.*?$")) "KEY = VALUE"

let regexp2 () =
  not (Pcre.pmatch ~rex:(Pcre.regexp ("^" ^ "KEY" ^ "\\s*=.*?$")) "MESSAGE TEXT")

let regexp3 () =
  (Re.get (Re.exec (Re_perl.compile_pat "\\d+") "X = 45") 0 = "45")

let run () =
  test "Regexp 1" regexp1;
  test "Regexp 2" regexp2;
  test "Regexp 3" regexp3


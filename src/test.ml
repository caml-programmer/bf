let test name call =
  Printf.printf "Test %s: %b\n" name (call ())

let write_lines file lines =
  let buf = Buffer.create 32 in
  let add = Buffer.add_string buf in
  let out s = add s; add "\n"; in
  List.iter out lines;
  System.write
    (Buffer.contents buf)
    file

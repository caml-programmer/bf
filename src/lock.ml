open Output

let openfile file =
  Unix.openfile file [Unix.O_CREAT;Unix.O_RDONLY;Unix.O_WRONLY] 0o644
       
let lock file =
  let msg = msg "Lock.lock" in
  msg "high" ("locking file '"^file^"'");
  Unix.lockf (openfile file) Unix.F_LOCK 1;
  
      

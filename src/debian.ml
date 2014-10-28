let fix_arch = function
  | "i686" -> "i386"
  | "i586" -> "i386"
  | s -> s

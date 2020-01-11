type p = K1 of bool*bool | K2 
let mm = function
  | K1 ((true|false), true) -> "tt"
  | K1 (false, false) -> "ff"
  | K1 (true, false) -> "tf"
  | K1 (false, true) -> "ft"
  | K2 -> "K2"

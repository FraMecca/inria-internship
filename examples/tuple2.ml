[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

type p = K1 of bool*bool | K2 
let mm = function
  | K1 ((true|false), true) -> observe true true
  | K1 (false, false) -> observe false false
  | K1 (true, false) -> observe true false
  | K1 (false, true) -> observe false true
  | K2 -> observe 0

[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let mm = function
  | (_, true) -> observe true
  | (_, false) -> observe false

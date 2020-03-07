[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let mm = function
  | (_, 2, _) -> observe 0 2 0
  | (_, _, 2) -> observe 0 0 2
  | (_, _, _) -> observe 0 0 0

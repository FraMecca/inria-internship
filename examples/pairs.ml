[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let f = function
  | (1, 2) -> observe 2

[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let f = function
  | true -> observe true
  | false -> observe false

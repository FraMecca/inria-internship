[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let f = function
  | true -> observe true
  | _ -> observe false

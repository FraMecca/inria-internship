[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let f = function
  | false -> observe false
  | _ -> observe true

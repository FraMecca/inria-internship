[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let mm = function
  | _ -> observe 0
  | 2 -> observe 2

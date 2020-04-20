[@@@warning "-20"]
external observe : 'a -> 'b = "observe"
external guard : 'a -> 'b = "guard"

let f = function
  | x when guard x -> observe x
  | x -> observe (-x)

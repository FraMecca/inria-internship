[@@@warning "-20"]
external observe : 'a -> 'b = "observe"
external guard : 'a -> 'b = "guard"

let f = function
  | _, x when guard x -> observe x
  | _, x -> observe 0

[@@@warning "-20"]
external observe : 'a -> 'b = "observe"
external guard : 'a -> 'b = "guard"

let ff = function
  | x when guard () -> observe 1
  | _ when guard 1 -> observe 2
  | _ -> observe 3

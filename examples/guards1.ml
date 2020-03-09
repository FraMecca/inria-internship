[@@@warning "-20"]
external observe : 'a -> 'b = "observe"
external guard : 'a -> 'b = "guard"

let ff = function
  | _ when guard () -> observe true
  | _ -> observe false

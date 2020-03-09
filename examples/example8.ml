[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

type 'a option = None | Some of 'a

let mm = function
  | (Some 0, true) -> observe (Some true)
  | (None, false) -> observe (Some false)
  | _ -> observe None


[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let mm = function
  | (_, _) -> observe 2
  | ((_, _), _) -> observe 3
  | ((_, _), (_, _)) -> observe 4

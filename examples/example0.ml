external observe : int -> int = "observe"

let mm = function
  | 2 -> observe 2
  | 3 -> observe 3
  | 4 -> observe 4
  | _ -> observe 5

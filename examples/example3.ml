[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

type t = K1 of int | K2 of bool | K3
let a = fun t -> match t with
  | K1 1 -> observe 1 1
  | K1 2 -> observe 1 2
  | K1 _ -> observe 1 ()
  | K2 true -> observe 2 true
  | K2 false -> observe 2 false
  | K3 -> observe 3

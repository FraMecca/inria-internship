[@@@warning "-20"]
external observe : 'a -> 'b = "observe"
external guard : 'a -> 'b = "guard"

type k1 = K1 of int

type k2 = K2 of int * int

let mm = function
  | (K1 1, _) -> observe 1
  | (K1 1, K2 (2, x)) when guard x -> observe 1 2
  | _ -> observe 0

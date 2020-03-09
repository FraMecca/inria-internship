[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

type p = P0 of int;;
type t = K1 of int | K2 of bool | K3 of int| K4 | K5 of p

let a = fun t -> match t with
  | K1 2 -> observe 1 2
  | K1 _ -> observe 1 ()
  | K2 b -> begin match b with
      | true -> observe 2 true
      | false -> observe 2 false
    end
  | K3 _ -> observe 3 ()
  | K4 -> observe 4
  | K5 (P0 1) -> observe 5 0 1
  | K5 (P0 2) -> observe 5 0 2
  | K5 _ -> observe 5 0 ()

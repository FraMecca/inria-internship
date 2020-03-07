[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

type t = K1 of int | K2 of bool | K3

let a = fun t -> match t with
  | K1 i -> begin match i with
      | 1 -> observe 1 1
      | 2 -> observe 1 2
      | 3 -> observe 1 3
      | _ -> observe 1 ()
    end
  | K2 true -> observe 2 true
  | K2 _ -> observe 2 false
  | _ -> observe 3

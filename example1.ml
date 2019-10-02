type p = P0 of int;;
type t = K1 of int | K2 of bool | K3 of int| K4 | K5 of p

let f t = "t"

let a = fun t -> match t with
  | K4 -> "K4"
  | K1 2 -> "K1 2"
  | K1 _ -> "K1 _"
  | _ -> "wildcard"

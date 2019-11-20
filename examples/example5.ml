type p = P0 of int;;
type t = K1 of int | K2 of bool | K3 of int| K4 | K5 of p

let a = fun t -> match t with
  | K1 2 -> "K1:2"
  | K1 _ -> "K1:_"
  | K2 b -> begin match b with
      | true -> "K2:true"
      | false -> "K2:false"
    end
  | K3 _ -> "K3:_"
  | K4 -> "K4"
  | K5 (P0 1) -> "K5:P0:1"
  | K5 (P0 2) -> "K5:P0:2"
  | K5 _ -> "K5:P0:_"

type t = K1 of int | K2 of bool | K3

let a = fun t -> match t with
  | K1 1 -> "K1:1"
  | K1 2 -> "K1:2"
  | K1 _ -> "K1:_"
  | K2 true -> "K2:true"
  | K2 false -> "K2:false"
  | K3 -> "K3"

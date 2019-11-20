type t = K1 of int | K2 of bool | K3

let a = fun t -> match t with
  | K1 i -> begin match i with
      | 1 -> "K1:1"
      | 2 -> "K1:2"
      | 3 -> "K1:3"
      | _ -> "K1:_"
    end
  | K2 true -> "K2:true"
  | K2 _ -> "K2:false"
  | _ -> "K3"

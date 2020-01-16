type k1 = K1 of int

type k2 = K2 of int * int

let mm = function
  | (K1 1, _) -> "k1_"
  | (K1 1, K2 (2, 2)) -> "k1k2"
  | _ -> "_"

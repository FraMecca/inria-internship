type t = K1 of int | K2 of bool | K3 of int| K4

let f t = "t"

let a = fun lt -> match lt with
  | [K1 0; _] as t -> f t  (* tag 0 *)
  | [K1 1; _] -> "K1 1" (* tag 0 *)
  | [_;K1 1] -> "K1 1" (* tag 0 *)
  | [_; K1 _] -> "K1 _" (* tag 0 *)
  | [K2 false; _] -> "K2 false"(* tag 1 *)
  | [K2 _; _] -> "K2 _"(* tag 1 *)
  | [_; K2 _] -> "K2 _"(* tag 1 *)
  | [K3 t] when t == 2 -> "K3 2"(* tag 2 *)
  | [K4] -> "K4" (* int 0 *)
  | _ -> "[_, _]"

external observe : 'a -> 'b = "observe"

let m t = match t with
  | _ -> observe ()

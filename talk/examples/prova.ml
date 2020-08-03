[@@@warning "-20"]
external observe : 'a -> 'b = "observe"
external guard : 'a -> 'b = "guard"

type 'a option = None | Some of 'a

let mm li = match li with
| [] -> observe (0, None)
| x::[] -> observe (1, Some x)
| x::y::_ when guard x y -> observe (2, Some y)

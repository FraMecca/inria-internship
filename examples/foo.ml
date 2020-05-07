[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

type t = Foo | Bar
type option = None | Some of t

let foo = function
| (None as x, None) -> observe (Some Foo)
| (None, (Some _ as x)) -> observe (Some Bar)
| _ -> observe None

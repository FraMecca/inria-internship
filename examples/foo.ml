[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

type t = Foo | Bar

let foo = function
| (None as x, None) -> observe (Some Foo)
| (None, (Some _ as x)) -> observe (Some Bar)
| _ -> observe None

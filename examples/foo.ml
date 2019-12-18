let foo = function
| (None as x, None) -> "foo"
| (None, (Some _ as x)) -> "bar"
| _ -> "_"

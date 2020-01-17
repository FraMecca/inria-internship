let mm = function
  | (Some 0, "whatever") -> "whatever"
  | (None, "none") -> "none"
  | _ -> "fallback"


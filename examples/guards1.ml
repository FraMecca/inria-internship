let ff = function
  | _ when Random.bool () -> "random.bool"
  | _ -> "wildcard"

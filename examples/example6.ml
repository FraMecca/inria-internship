[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let mm = function
  | [(1, 2), 2]::[] -> observe ([(1, 2), 2]::[])
  | [(1, 2), 2]::_ -> observe [(1, 2), 2]
  | [(1, 2), x]::_ -> observe [(1, 2), x]
  | [x]::[] -> observe [x]
  | [x]::[y]::[] -> observe [x, y]
  | [x, y]::z -> observe [(x, y), z]
  | x -> observe [x]

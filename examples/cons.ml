[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let mm = function
  | [1;2;3] -> observe [1; 2; 3]
  | [0;0;0] -> observe [0; 0; 0]
  | [2;2;2] -> observe [2; 2; 2]
  | [3;3;3] -> observe [3; 3; 3]
  | _ -> observe []

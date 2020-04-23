[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let test = function
  | true -> observe 0
  (* we expect a Match_failure node for 'false' in the lambda representation *)

[@@@warning "-20"]
external observe : 'a -> 'b = "observe"

let test = function
  | true -> observe 0
  | false -> observe 1
  | _ -> .
    (* Unreachable; if this annotation was incorrect,
       the OCaml compiler would error at pattern-checking-time *)

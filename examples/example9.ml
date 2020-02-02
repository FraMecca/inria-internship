let test = function
  | true -> 0
  | false -> 1
  | _ -> .
    (* Unreachable; if this annotation was incorrect,
       the OCaml compiler would error at pattern-checking-time *)

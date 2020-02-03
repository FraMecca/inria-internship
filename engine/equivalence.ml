open Target_sym_engine

let satisfy_pi pi pi' =
  if pi.var <> pi'.var then
    false
  else
    match pi.op with
    | Tag i ->
      begin match pi'.op with
        | Tag i' -> i = i'
        | NotTag i' -> i <> i'
        | _ -> false
      end
    | NotTag i -> 
      begin match pi'.op with
        | Tag i' -> i <> i'
        | NotTag i' -> i = i'
        | _ -> false
      end
    | Int i ->
      begin match pi'.op with
        | Int i' -> BatIO.write_line BatIO.stdout ("Testing: "^(string_of_int i)^":"^(string_of_int i'));
          i = i'
        | NotInt i' -> i <> i'
        | _ -> false
      end
    | NotInt i ->
      begin match pi'.op with
        | Int i' -> i <> i'
        | NotInt i' -> i = i'
        | _ -> false
      end
    | Ge i ->
      begin match pi'.op with
        | Int i' -> i >= i'
        | _ -> false
      end
    | Gt i ->
      begin match pi'.op with
        | Int i' -> i < i'
        | _ -> false
      end
    | Le i ->
      begin match pi'.op with
        | Int i' -> i <= i'
        | _ -> false
      end
    | Lt i ->
      begin match pi'.op with
        | Int i' -> i < i'
        | _ -> false
      end
    | Eq i ->
      begin match pi'.op with
        | Eq i' -> i = i'
        | Nq i' -> i <> i'
        | _ -> false
      end
    | Nq i ->
      begin match pi'.op with
        | Eq i' -> i <> i'
        | Nq i' -> i = i'
        | _ -> false
      end
    | Isout i ->
      begin match pi'.op with
        | Isout i' -> i = i'
        | _ -> false
      end
    | Isin i -> 
      begin match pi'.op with
        | Isin i' -> i = i'
        | _ -> false
      end

let rec eq_checker (src: constraint_tree) (dst: constraint_tree) : bool =
  let check_constraints = function
    | (pisrc::[], pidst::[]) -> satisfy_pi pisrc pidst
    | (_::_, _::_) -> assert false (* TODO: DISCUSS do we still need lists? *)
    | _ -> assert false
  in
  let rec right_branch constraints (constraints', (tree: constraint_tree)) =
    if check_constraints (constraints, constraints') then
      true
    else
      match tree with
      | Node children -> children
                         |> List.map (fun child -> right_branch constraints child)
                         |> List.exists (Bool.equal true)
      |_ -> assert false
  in
  let rec trim (constraints: pi list) = function
    | Node [] -> assert false
    | Leaf _ -> None
    | Failure -> None
    | Node (this::rest) ->
      if right_branch constraints this then
        Some (snd this)
      else
        trim constraints (Node rest)
  in
  match (src, dst) with
  | (Node [], _) -> assert false
  | (Leaf tbox, Leaf tbox') when tbox = tbox' -> true
  | (Failure, Failure) -> true
  | Node children, Node _ ->
    children
    |> List.map (fun (pilst, child) ->
        let dst' = trim pilst dst in
        match dst' with
        | Some tree -> eq_checker child tree
        | None -> false)
    |> List.exists (Bool.equal false)
    |> not
  | _ -> false

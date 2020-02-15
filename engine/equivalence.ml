type specialized_tree =
  | Terminal of Merge_accessors.constraint_tree
  | SNode of (node_constraint * specialized_tree) list * (fallback_constraints * specialized_tree) option
and
  node_constraint =
  | Symbolic of Merge_accessors.pi
  | Resolved of bool
and
  fallback_constraints =
  | FSymbolic of Merge_accessors.pi list
  | FResolved of bool



let specialize (pi: Merge_accessors.pi) (pi': Merge_accessors.pi) : node_constraint = 
  if pi.var <> pi'.var then
    Symbolic pi'
  else
    match pi.op with
    | Tag i ->
      begin match pi'.op with
        | Tag i' -> Resolved (i = i')
        | NotTag i' -> Resolved (i <> i')
        | _ -> Resolved false
      end
    | NotTag i -> 
      begin match pi'.op with
        | Tag i' -> Resolved (i <> i')
        | NotTag i' -> Resolved (i = i')
        | _ -> Resolved false
      end
    | Int i ->
      begin match pi'.op with
        | Int i' -> Resolved (i = i')
        | NotInt i' -> Resolved (i <> i')
        | _ -> Resolved false
      end
    | NotInt i ->
      begin match pi'.op with
        | Int i' -> Resolved (i <> i')
        | NotInt i' -> Resolved (i = i')
        | _ -> Resolved false
      end
    | Ge i ->
      begin match pi'.op with
        | Int i' -> Resolved (i >= i')
        | _ -> Resolved false
      end
    | Gt i ->
      begin match pi'.op with
        | Int i' -> Resolved (i < i')
        | _ -> Resolved false
      end
    | Le i ->
      begin match pi'.op with
        | Int i' -> Resolved (i <= i')
        | _ -> Resolved false
      end
    | Lt i ->
      begin match pi'.op with
        | Int i' -> Resolved (i < i')
        | _ -> Resolved false
      end
    | Eq i ->
      begin match pi'.op with
        | Eq i' -> Resolved (i = i')
        | Nq i' -> Resolved (i <> i')
        | _ -> Resolved false
      end
    | Nq i ->
      begin match pi'.op with
        | Eq i' -> Resolved (i <> i')
        | Nq i' -> Resolved (i = i')
        | _ -> Resolved false
      end
    | Isout i ->
      begin match pi'.op with
        | Isout i' -> Resolved (i = i')
        | _ -> Resolved false
      end
    | Isin i -> 
      begin match pi'.op with
        | Isin i' -> Resolved (i = i')
        | _ -> Resolved false
      end

let reduce_fallback_constraints constraints = function
  | FResolved false -> FResolved false
  | FResolved true -> FResolved true
  | FSymbolic pilst ->
    let fpi' = pilst
               |> List.map (fun pi -> specialize constraints pi)
               |> List.filter (fun spi -> spi <> Resolved true)
    in
    match fpi' with
    | [] -> FResolved true
    | pilst when pilst |> List.exists (fun pi -> pi <> (Resolved false)) -> FResolved false
    | pilst -> let fmap = List.map (fun pi -> match pi with
        | Resolved _ -> assert false
        | Symbolic pi -> pi)
      in
      FSymbolic (fmap pilst)

let compare (src: Merge_accessors.constraint_tree) (dst: Merge_accessors.constraint_tree) : bool =
  let rec init : Merge_accessors.constraint_tree -> specialized_tree = function
    | (Leaf _ | Failure) as t -> Terminal t
    | Node (children, fallback) ->
      let c = children |> List.map (fun (pi, c_tree) -> Symbolic pi, init c_tree) in
      let fb = fallback |> Option.map (fun (pilst, c_tree) -> FSymbolic pilst, init c_tree) in
      SNode (c, fb)
  in
  let rec _compare (src: Merge_accessors.constraint_tree) (dst: specialized_tree) : bool =
    let rec trim constraints = function
      | SNode ([], None) -> assert false
      | Terminal c -> Terminal c
      | SNode (children, fallback) ->
        let children' =
          children
          |> List.filter_map (fun (pi, s_tree) ->
              match pi with
              | Resolved false -> None
              | Resolved true -> Some (pi, trim constraints s_tree)
              | Symbolic pi -> Some (specialize constraints pi, trim constraints s_tree))
        in
        let fallback' =
          match fallback with
          | None -> None
          | Some (fpi, s_tree) ->
            match reduce_fallback_constraints constraints fpi with
            | FResolved false -> None (* discard it *)
            | fpi' -> Some (fpi', trim constraints s_tree)
        in
        SNode (children', fallback')
    in
    let rec compare_terminals : Merge_accessors.constraint_tree * specialized_tree -> bool = function
      | (Leaf tbox, Terminal (Leaf tbox')) ->
        tbox = tbox'
      | (Failure, Terminal Failure) ->
        true
      | (Leaf tbox, SNode ((Resolved true, s_tree)::[], None)) ->
        compare_terminals (Leaf tbox, s_tree)
      | (Leaf tbox, SNode ([], Some (FResolved true, fallback))) ->
        compare_terminals (Leaf tbox, fallback)
      | _ ->
        false
    in
    match (src, dst) with
    | ((Leaf _ | Failure), _) -> compare_terminals (src, dst)
    | Node (children, _fallback), SNode _ ->
      children
      |> List.map (fun (pi, child) ->
          let dst' = trim pi dst in
          _compare child dst')
      |> List.exists (Bool.equal false)
      |> not
    | _ -> false
  in
  _compare src (init dst)

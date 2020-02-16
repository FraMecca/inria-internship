open Merge_accessors

type specialized_tree =
  | Terminal of constraint_tree
  | SNode of sym_value * (node_constraint * specialized_tree) list * (node_constraint * specialized_tree) option
and
  node_constraint =
  | Symbolic of domain
  | Resolved of bool

let compare (src: Merge_accessors.constraint_tree) (dst: Merge_accessors.constraint_tree) : bool =
  let rec init : Merge_accessors.constraint_tree -> specialized_tree = function
    | (Leaf _ | Failure) as t -> Terminal t
    | Node (acc, children, fallback) ->
      let c = children |> List.map (fun (pi, c_tree) -> Symbolic pi, init c_tree) in
      let fb = fallback |> Option.map (fun (pi, c_tree) -> Symbolic pi, init c_tree) in
      SNode (acc, c, fb)
  in
  let specialize (pi: domain) (pi': domain) : node_constraint =
    let intersect = Target_sym_engine.Domain.inter in
    let is_empty (set:domain) =
      Target_sym_engine.Domain.Set.is_empty set.int && Target_sym_engine.Domain.Set.is_empty set.tag
    in
    match intersect pi pi' with
    | x when x = pi' ->
      Resolved true
    | x when is_empty x ->
      Resolved false
    | x -> Symbolic x
  in
  let rec _compare (src: Merge_accessors.constraint_tree) (dst: specialized_tree) : bool =
    let rec trim src_acc src_constraints = function
      | SNode (_, [], None) -> assert false
      | Terminal c -> Terminal c
      | SNode (acc, children, fallback) ->
        let children' =
          children
          |> List.filter_map (fun (pi, s_tree) ->
              match pi with
              | Resolved false -> None
              | Resolved true -> Some (pi, trim src_acc src_constraints s_tree)
              | Symbolic dst_constraint ->
                if src_acc = acc then
                  Some (specialize src_constraints dst_constraint, trim src_acc src_constraints s_tree)
                else
                  Some (pi, trim src_acc src_constraints s_tree))
        in
        let fallback' =
          match fallback with
          | None -> None
          | Some (pi, s_tree) ->
            match pi with
            | Resolved false ->
              None
            | Resolved true ->
              Some (pi, trim src_acc src_constraints s_tree)
            | Symbolic pi ->
              Some (specialize src_constraints pi, trim src_acc src_constraints s_tree)
        in
        SNode (acc, children', fallback')
    in
    let rec compare_terminals : constraint_tree * specialized_tree -> bool = function
      | (Leaf tbox, Terminal (Leaf tbox')) ->
        tbox = tbox'
      | (Failure, Terminal Failure) ->
        true
      | (Leaf tbox, SNode (_, (Resolved true, s_tree)::[], None)) ->
        compare_terminals (Leaf tbox, s_tree)
      | (Leaf tbox, SNode (_, [], Some (Resolved true, fallback))) ->
        compare_terminals (Leaf tbox, fallback)
      | _ ->
        false
    in
    match (src, dst) with
    | ((Leaf _ | Failure), _) -> compare_terminals (src, dst)
    | Node (acc, children, _fallback), SNode _ ->
      children
      |> List.map (fun (pi, child) ->
          let dst' = trim acc pi dst in
          _compare child dst')
      |> List.exists (Bool.equal false)
      |> not
    | _ -> false
  in
  _compare src (init dst)

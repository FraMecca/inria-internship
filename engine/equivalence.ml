open Merge_accessors

module Domain = Target_sym_engine.Domain

let compare (left: Merge_accessors.constraint_tree) (right: Merge_accessors.constraint_tree) : bool =
  let specialize (pi: domain) (pi': domain) : domain =
    Domain.inter pi pi'
  in
  let rec _compare (left: Merge_accessors.constraint_tree) (right: Merge_accessors.constraint_tree) : bool =
    let rec trim src_acc constraints =
      let specialize_same_acc  node_acc (dom, s_tree) =
        let dom' =
          if src_acc <> node_acc then dom
          else specialize constraints dom in
        if Domain.is_empty dom' then None
        else Some (dom', trim src_acc constraints s_tree)
      in
      function
      | Node (_, [], None) -> assert false
      | Failure -> Failure
      | Leaf l -> Leaf l
      | Guard (_, _, _) -> failwith "Not implemented"
      | Node (node_acc, children, fallback) ->
        let children' =
          children
          |> List.filter_map (specialize_same_acc node_acc) 
        in
        let fallback' =
          Option.bind fallback (specialize_same_acc node_acc)
        in
        Node (node_acc, children', fallback')
    in
    match (left, right) with
    | Node (acc, children, fallback), _ ->
      Option.to_list fallback @ children
      |> List.for_all (fun (pi, child) ->
          _compare child (trim acc pi right))
    | _ , Node (acc, children, fallback) -> 
      Option.to_list fallback @ children
      |> List.for_all (fun (pi, child) ->
          _compare (trim acc pi left) child)
    | (Leaf tbox, Leaf tbox') ->
      tbox = tbox'
    | (Failure, Failure) -> true
    | (Failure, Leaf _) | (Leaf _, Failure) -> false
    | Guard (_, _, _), _ | _, Guard (_, _, _)-> failwith "Not implemented"
  in
  _compare left right

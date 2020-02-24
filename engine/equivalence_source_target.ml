open Merge_accessors
open Ast

module Domain = Target_sym_engine.Domain

type source_tree = Source_sym_engine.constraint_tree
type target_tree = Merge_accessors.constraint_tree

type source_constructor = Ast.constructor

let constructor_to_domain (_: source_constructor) : Domain.t = assert false
let fallback_constructor (_: source_tree) : source_constructor = assert false

let compare (left: source_tree) (right: target_tree) : bool =
  let specialize (k: source_constructor) (pi': domain) : domain =
    let pi = constructor_to_domain k in
    Domain.inter pi pi'
  in
  let rec _compare (left: source_tree) (right: target_tree) : bool =
    let rec trim src_acc (constructor: source_constructor) =
      let specialize_same_acc  node_acc (dom, s_tree) =
        if Domain.is_empty dom then
          None
        else if src_acc = node_acc then
          Some (specialize constructor dom, trim src_acc constructor s_tree)
        else
          Some (dom, trim src_acc constructor s_tree)
      in
      function
      | Node (_, [], None) -> failwith "Shouldn't happen: Node with no branches and no fallback case"
      | Failure -> Failure
      | Leaf l -> Leaf l
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
    let input_space = assert false in (* TODO: DISCUSS *)
    if Domain.equal input_space Domain.empty then
      true
    else
      match (left, right) with
      | ((Failure | Leaf _) as terminal, Node (_, children, fallback)) ->
        Option.to_list fallback @ children
        |> List.for_all (fun (_, child) -> _compare terminal child)
      | Node (acc, children, fallback), _ ->
        let branches = match fallback with
          | Unreachable -> children
          | _ -> (fallback_constructor left, fallback) :: children in
        List.for_all (fun (kst, child) ->  _compare child (trim acc kst right)) branches
      | (Failure, Failure) -> true
      | (Leaf (SBlackbox slf), Leaf rlf) -> slf = rlf (* blackbox comparison is simply string eq *)
      | (Failure, Leaf _) | (Leaf _, Failure) | (Unreachable, _) ->
        false
  in
  _compare left right

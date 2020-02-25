open Merge_accessors
open Ast

module Domain = Target_sym_engine.Domain

module AcMap = Map.Make(
  struct type t = accessor
    let rec compare a a' =
      match (a, a') with
      | (AcRoot, AcRoot) -> 0
      | (AcRoot, AcField _) -> -1
      | (AcField _, AcRoot) -> 1
      | (AcField (a, i), AcField (a', i')) when a = a' -> Int.compare i i'
      | (AcField (a, _), AcField (a', _)) -> compare a a'
  end)

type source_tree = Source_sym_engine.constraint_tree
type target_tree = Merge_accessors.constraint_tree

type source_constructor = Ast.constructor

let constructor_to_domain (_: source_constructor) : Domain.t = assert false
let fallback_domain (_: source_tree) : domain = assert false

let compare (left: source_tree) (right: target_tree) : bool =
  let rec trim src_acc src_pi =
    let specialize_same_acc  node_acc (pi, s_tree) =
      if Domain.is_empty pi then
        None
      else if src_acc = node_acc then
        Some (Domain.inter src_pi pi, trim src_acc src_pi s_tree)
      else
        Some (pi, trim src_acc src_pi s_tree)
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
  let dead_end input_space =
    AcMap.for_all (fun _ value -> not (Domain.is_empty value)) input_space
  in
  let rec compare_ (left: source_tree) (right: target_tree) (input_space: domain AcMap.t) : bool =
    if dead_end input_space then
      true
    else
      match (left, right) with
      | ((Failure | Leaf _) as terminal, Node (_, children, fallback)) ->
        Option.to_list fallback @ children
        |> List.for_all (fun (_, child) -> compare_ terminal child input_space)
      | Node (acc, children, fallback), _ ->
        let compare_branch (pi, branch) =
          let input_pi = AcMap.find acc input_space in
          let input_space' = AcMap.add acc (Domain.inter input_pi pi) input_space in
          compare_ branch (trim acc pi right) input_space'
        in
        let children' = List.map (fun (kst, tree) -> (constructor_to_domain kst, tree)) children in
        let branches = match fallback with
          | Unreachable -> children'
          | _ -> (fallback_domain left, fallback) :: children' in
        List.for_all compare_branch branches
      | (Unreachable, _) ->
        prerr_endline "Warning: unreachable branch";
        true
      | (Failure, Failure) -> true
      | (Leaf (SBlackbox slf), Leaf rlf) -> slf = rlf (* blackbox comparison is simply string eq *)
      | (Failure, Leaf _) | (Leaf _, Failure) ->
        false
  in
  compare_ left right AcMap.empty

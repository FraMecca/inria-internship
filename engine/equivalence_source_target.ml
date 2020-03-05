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
type sym_values = Source_sym_engine.sym_value list

let target_value_from_source_sym_value _ = assert false

let constructor_to_domain repr_env : constructor -> domain = function
  | Int i -> Domain.int (Domain.Set.point i)
  | Bool false -> Domain.int (Domain.Set.point 0)
  | Bool true -> Domain.int (Domain.Set.point 1)
  | String _ -> assert false (* Still not handled *)
  | Nil -> Domain.int (Domain.Set.point 0)
  | Cons | Tuple _ -> Domain.tag (Domain.Set.point 0)
  | Variant v ->
    let open Source_env in
    match ConstructorMap.find v repr_env with
    | Int i -> Domain.int (Domain.Set.point i)
    | Tag t -> Domain.tag (Domain.Set.point t)

let constrained_subtrees repr_env children fallback =
  let children' = List.map (fun (kst, tree) ->
      (constructor_to_domain repr_env kst, tree)) children in
  let fb_domain =
    children'
    |> List.map fst
    |> List.fold_left Domain.union Domain.empty
    |> Domain.negate in
  ((fb_domain, fallback) :: children')

let tag_of_constructor : source_constructor -> int = assert false (* TODO: discuss *)
  
let compare (repr_env: Source_env.type_repr_env) (left: source_tree) (right: target_tree) : bool =
  let rec sym_values_eq  (sym_values:sym_values) (target_values:target_value list): bool =
    let equal_ : (Source_sym_engine.sym_value * target_value) -> bool = function
      | (SAccessor acc, VAccessor acc') -> acc = acc'
      | (SCons (Int i, []) , VConstant i') -> i = i'
      | (SCons (k, rest), VConstructor {tag=t; args=rest'}) ->
        (tag_of_constructor k) = t && sym_values_eq rest rest'         
      | _ -> false
    in
    match (sym_values, target_values) with
    | (shd::stl,  thd::ttl) when equal_ (shd, thd) -> sym_values_eq stl ttl
    | ([], []) -> true
    | _ -> false
  in
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
    | Guard (bb, ctrue, cfalse) -> Guard (bb, ctrue, cfalse)
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
  let specialize_input_space acc pi input_space =
    let pi' = match AcMap.find_opt acc input_space with
      | Some input_pi -> Domain.inter input_pi pi
      | None -> pi
    in
    AcMap.add acc pi' input_space
  in
  let dead_end input_space =
    AcMap.exists (fun _ value -> Domain.is_empty value) input_space
  in
  let rec compare_ (input_space: domain AcMap.t) (guards: sym_values list) (left: source_tree) (right: target_tree) : bool =
    if dead_end input_space then
      true
    else
      match (left, right) with
      | Node (acc, children, fallback), _ ->
        let compare_branch (pi, branch) =
          let input_space' = specialize_input_space acc pi input_space in
          compare_ input_space' guards branch (trim acc pi right)
        in
        constrained_subtrees repr_env children fallback
        |> List.for_all compare_branch
      | ((Failure | Leaf _) as terminal, Node (_, children, fallback)) ->
        Option.to_list fallback @ children
        |> List.for_all (fun (_, child) -> compare_ input_space guards terminal child)
      | (Unreachable, _) ->
        prerr_endline "Warning: unreachable branch";
        true
      | (Guard (svl, ctrue, cfalse), _) ->
         let guards' = guards@[svl] in
         compare_ input_space guards' ctrue right && compare_ input_space guards' cfalse right
      | (_, Guard (tvl, ctrue, cfalse)) ->
        begin match guards with
          | hd::grest when sym_values_eq hd tvl ->
            compare_ input_space grest left ctrue && compare_ input_space grest left cfalse
          | _ -> false
        end
      | (Failure, Failure) -> guards = []
      | (Leaf slf, Leaf rlf) -> guards = [] && sym_values_eq slf rlf
      | (Failure, Leaf _) | (Leaf _, Failure) ->
        false
  in
  compare_ AcMap.empty [] left right

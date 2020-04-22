open Ast

type constraint_tree =
  | Failure
  | Leaf of sym_value list
  | Guard of sym_value list * constraint_tree * constraint_tree
  | Node of accessor * (domain * constraint_tree) list * (domain * constraint_tree) option
and
  sym_value =
  | VConstructor of {tag:int; args:sym_value list}
  | VAccessor of accessor
  | VConstant of int
and
  domain = Target_domain.t
and
  sym_function = variable * constraint_tree
and
  sym_catch = exitpoint * variable list * constraint_tree

let rec merge : Target_sym_engine.constraint_tree -> constraint_tree =
  let rec map_accessor : Target_sym_engine.accessor -> accessor = function
    | AcRoot _ -> AcRoot
    | AcField (s, i) -> AcField(map_accessor s, i)
    | AcAdd (_, _) -> assert false
  in
  let rec map_target_value : Target_sym_engine.sym_value -> sym_value = function
    | VConstant i -> VConstant i
    | VConstructor {tag=t; args=args} -> VConstructor {tag=t; args=List.map map_target_value args}
    | VAccessor acc -> VAccessor (map_accessor acc)
  in
  let rec split : Target_sym_engine.accessor -> accessor * int = function
    | AcAdd (s, i) ->
       let (s, offset) = split s in (s, offset+i)
    | other -> (map_accessor other, 0)
  in
  let shift_domain offset domain =
    let open Target_domain in
    (* Note: AcAdd is only use on integer concrete values,
       not tags. Thus, we only shift the set of possible integer values,
       and preserve the set of possible tags. *)
    {
      int = Set.shift (-offset) domain.int;
      tag = domain.tag;
    } in
  function
  | Target_sym_engine.Failure ->
    Failure
  | Target_sym_engine.Leaf tvl ->
    let tvl' = List.map map_target_value tvl in
    Leaf tvl'
  | Target_sym_engine.Guard (tvl, ctrue, cfalse) ->
    let tvl' = List.map map_target_value tvl in
    Guard (tvl', merge ctrue, merge cfalse)
  | Target_sym_engine.Node (var, children, fallback) ->
    let (var, offset) = split var in
    let subst (dom, c_tree) = (shift_domain offset dom, merge c_tree) in
    Node (var, List.map subst children, Option.map subst fallback)

let print_tree tree =
 let module Domain = Target_domain in
  let bprintf = Printf.bprintf
  in
  let rec bprint_svalue buf = function
    | AcRoot -> bprintf buf "AcRoot="
    | AcField (a, i) -> bprintf buf "AcField(%d %a)" i bprint_svalue a
  in
  let bprint_pi buf (var, domain) =
    bprintf buf "{ var=%a; dom=%a; }" bprint_svalue var Domain.bprint domain
  in
  let rec bprint_list ~sep bprint buf = function
    | [] -> ()
    | [x] -> bprint buf x
    | x :: xs ->
      bprintf buf "%a%t%a"
        bprint x
        sep
        (bprint_list ~sep bprint) xs in
  let indent ntabs buf =
    bprintf buf "%s" (List.init ntabs (fun _ -> "\t") |> String.concat "")
  in
  let _break ntabs buf =
    bprintf buf "\n%t" (indent ntabs)
  in
  let comma buf = bprintf buf ", " in
  let rec bprint_accessor buf = function
    | AcRoot -> bprintf buf "AcRoot"
    | AcField (a, i) -> bprintf buf "AcFiled %a.%d" bprint_accessor a i
  in
  let rec bprint_sym_value buf = function
    | VAccessor acc -> bprintf buf "VAccessor:%a" bprint_accessor acc
    | VConstant i -> bprintf buf "VConstant:%d" i
    | VConstructor {tag=t; args=a} -> bprintf buf "VConstructor:{tag=%d; args=%a}"
                                        t
                                        (bprint_list ~sep:comma bprint_sym_value) a
  in
  let rec bprint_tree ntabs buf tree =
    match tree with
    | Failure ->
      bprintf buf "%tFailure" (indent ntabs)
    | Leaf observe ->
      bprintf buf "%tLeaf=%a\n" (indent ntabs)
        (bprint_list ~sep:comma bprint_sym_value) observe
    | Guard (tgt_values, ctrue, cfalse) ->
      let bprint_child prefix tree =
        bprintf buf
          "%t%s =\n%a"
          (indent ntabs)
          prefix
          (bprint_tree (ntabs+1)) tree
      in
      bprintf buf "%tGuard (%a):\n" (indent ntabs)
        (bprint_list ~sep:comma bprint_sym_value) tgt_values;
      bprint_child "guard(true)" ctrue;
      bprint_child "guard(false)" cfalse
    | Node (var, children, fallback) ->
      let bprint_child buf (domain, tree) =
        bprintf buf
          "%tNode (%a) =\n%a"
          (indent ntabs)
          bprint_pi ( var, domain )
          (bprint_tree (ntabs+1)) tree
      in
      bprint_list ~sep:ignore bprint_child buf children;
      match fallback with
      | Some (domain, tree) ->
        bprintf buf "%tFallback=Node (%a) =\n%a"
          (indent ntabs)
          bprint_pi (var, domain)
          (bprint_tree (ntabs+1)) tree
      | None -> bprintf buf "%tFallback=None\n" (indent ntabs)
  in
  let buf = Buffer.create 42 in
  bprint_tree 0 buf tree;
  BatIO.write_line BatIO.stdout (Buffer.contents buf)


let rec accessor_to_string = function
  | AcRoot -> "AcRoot"
  | AcField (a, i) -> "AcField "^accessor_to_string a^"."^string_of_int i

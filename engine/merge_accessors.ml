open Ast

type constraint_tree =
  | Failure
  | Leaf of Ast.target_blackbox
  | Node of sym_value * (domain * constraint_tree) list * (domain * constraint_tree) option
and
  domain = Target_sym_engine.domain
and
  sym_function = variable * constraint_tree
and
  sym_catch = exitpoint * variable list * constraint_tree
and
  sym_value =
  | AcRoot of variable
  | AcField of sym_value * int
  | AcTag of sym_value * int

let rec merge : Target_sym_engine.constraint_tree -> constraint_tree =
  let rec map_sym_value : Target_sym_engine.sym_value -> sym_value = function
    | AcRoot v -> AcRoot v
    | AcField (s, i) -> AcField(map_sym_value s, i)
    | AcTag (s, i) -> AcTag(map_sym_value s, i)
    | AcAdd (_, _) -> assert false
  in
  let rec split : Target_sym_engine.sym_value -> sym_value * int = function
    | AcAdd (s, i) ->
       let (s, offset) = split s in (s, offset+i)
    | other -> (map_sym_value other, 0)
  in
  let shift_domain offset domain =
    let open Target_sym_engine in
    (* Note: AcAdd is only use on integer concrete values,
       not tags. Thus, we only shift the set of possible integer values,
       and preserve the set of possible tags. *)
    {
      int = Domain.Set.shift (-offset) domain.int;
      tag = domain.tag;
    } in
  function
  | Target_sym_engine.Failure -> Failure
  | Target_sym_engine.Leaf l -> Leaf l
  | Target_sym_engine.Node (var, children, fallback) ->
    let (var, offset) = split var in
    let subst (dom, c_tree) = (shift_domain offset dom, merge c_tree) in
    Node (var, List.map subst children, Option.map subst fallback)

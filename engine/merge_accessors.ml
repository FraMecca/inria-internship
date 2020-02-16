open Ast

type constraint_tree =
  | Failure
  | Leaf of Ast.target_blackbox
  | Node of accessor * (domain * constraint_tree) list * (domain * constraint_tree) option
and
  domain = Target_sym_engine.domain
and
  sym_function = variable * constraint_tree
and
  sym_catch = exitpoint * variable list * constraint_tree
and
  accessor =
  | AcRoot of variable
  | AcField of accessor * int
  | AcTag of accessor * int

let rec merge : Target_sym_engine.constraint_tree -> constraint_tree =
  let rec map_accessor : Target_sym_engine.accessor -> accessor = function
    | AcRoot v -> AcRoot v
    | AcField (s, i) -> AcField(map_accessor s, i)
    | AcTag (s, i) -> AcTag(map_accessor s, i)
    | AcAdd (_, _) -> assert false
  in
  let rec split : Target_sym_engine.accessor -> accessor * int = function
    | AcAdd (s, i) ->
       let (s, offset) = split s in (s, offset+i)
    | other -> (map_accessor other, 0)
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

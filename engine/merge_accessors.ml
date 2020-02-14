open Ast

type constraint_tree =
  | Failure
  | Leaf of Ast.target_blackbox
  | Node of (pi * constraint_tree) list * (pi list * constraint_tree) option
and
  pi = { var: sym_value; op: Target_sym_engine.piop }
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
  let rec map_sym_value (pi: Target_sym_engine.pi) : pi =
    match pi.var with
    | AcRoot v -> {var=AcRoot v; op=pi.op}
    | AcField (s, i) ->
      let inner = map_sym_value {var=s; op=pi.op} in
      {var=AcField (inner.var, i); op=inner.op}
    | AcTag (s, i) ->
      let inner = map_sym_value {var=s; op=pi.op} in
      {var=AcTag (inner.var, i); op=inner.op}
    | AcAdd (_, _) -> assert false
  in
  let rec subst_acadd (pi: Target_sym_engine.pi) : pi =
    match pi.var with
    | AcAdd (svalue, a) -> let op':Target_sym_engine.piop =  match pi.op with
        | Tag i -> Tag (a+i)
        | NotTag i -> NotTag (a+i)
        | Int i -> Int (a+i)
        | NotInt i -> NotInt (a+i)
        | Ge i -> Ge (a+i)
        | Gt i -> Gt (a+i)
        | Le i -> Le (a+i)
        | Lt i -> Lt (a+i)
        | Eq i -> Eq (a+i)
        | Nq i -> Nq (a+i)
        | Isout i -> Isout (a+i)
        | Isin i -> Isin (a+i)
      in
      subst_acadd {var=svalue; op=op'}
    | _ -> map_sym_value pi
  in
  function
  | Target_sym_engine.Failure -> Failure
  | Target_sym_engine.Leaf l -> Leaf l
  | Target_sym_engine.Node (children, fallback) -> 
    let subst_children = List.map (fun (pi, c_tree) -> (subst_acadd pi, merge c_tree))
    in
    let subst_fallback = Option.map (fun (pilst, c_tree) ->
        (pilst |> List.map subst_acadd, merge c_tree))
    in
    Node (subst_children children, subst_fallback fallback)

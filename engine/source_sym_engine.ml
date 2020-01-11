open Ast

type constraint_tree =
  | Leaf of pi list * source_expr
  | Node of pi * constraint_tree list
  | Conjunction of constraint_tree list
  | Root of constraint_tree list
and
  pi = { idx: int; op: piop }
and
  piop =
  | IsK of string (* Variant *)
  | NotK of string (* All variants except k *)
  | IsTrue 
  | IsFalse 
  | IsTuple 
  | NotTuple 
  | IsNil
  | NotNil
  | IsCons
  | NotCons
  | EqInt of int
  | NeqInt of int
  | EqString of string
  | NeqString of string

let print_result stree =
  let bprintf = Printf.bprintf
  in
  let bprint_piop buf piop = match piop with
    | EqInt i -> bprintf buf "EqInt %d" i
    | NeqInt i -> bprintf buf "NeqInt %d" i
    | IsK s -> bprintf buf "IsK %s" s
    | NotK s -> bprintf buf "NotK %s" s
    | IsTrue -> bprintf buf "IsTrue"
    | IsFalse -> bprintf buf "IsFalse"
    | IsTuple -> bprintf buf "IsTuple"
    | NotTuple -> bprintf buf "NotTuple"
    | IsNil -> bprintf buf "IsNil"
    | NotNil -> bprintf buf "NotNil"
    | IsCons -> bprintf buf "IsCons"
    | NotCons -> bprintf buf "NotCons"
    | EqString s -> bprintf buf "EqString %s" s
    | NeqString s -> bprintf buf "NeqString %s" s
  in
  let bprint_pi buf pi =
    bprintf buf "{idx=%d; %a}"
      pi.idx
      bprint_piop pi.op
  in
  let bprint_source_expr buf = function SBlackbox s -> bprintf buf "%s" s in
  let rec bprint_list ~sep bprint buf = function
    | [] -> ()
    | [x] -> bprint buf x
    | x :: xs ->
       bprintf buf "%a%t%a"
         bprint x
         sep
         (bprint_list ~sep bprint) xs in
  let break ntabs buf =
    bprintf buf "\n%s" (BatList.init ntabs (fun _ -> "\t") |> String.concat "") in
  let rec bprint_tree ntabs buf tree =
    let sep = break (ntabs+1) in
    match tree with
    | Leaf (pilist, expr) ->
      bprintf buf
        "Leaf='%a':\
         %t"
        bprint_source_expr expr
        (break (ntabs+1)) ;
      bprintf buf "constraints {%a}" (bprint_list ~sep bprint_pi) pilist
    | Node (pi, cst_list) ->
      bprintf buf
         "Node: constraints {%a}\
          %t%a"
         bprint_pi pi
         (break (ntabs+1))
         (bprint_list ~sep (bprint_tree (ntabs+1))) cst_list
    | Conjunction (cst_list) ->
      bprintf buf
         "Conjunction:%t%a"
         (break (ntabs+1))
         (bprint_list ~sep (bprint_tree (ntabs+1))) cst_list
    | Root cst_list ->
       bprintf buf
         "Root=\
          %t%a"
         (break (ntabs+1))
         (bprint_list ~sep (bprint_tree (ntabs+1))) cst_list
  in
  let buf = Buffer.create 42 in
  bprint_tree 0 buf stree;
  BatIO.write_line BatIO.stdout (Buffer.contents buf)

let sym_exec (source: source_program) =
  let shares_kst ?wildcard_to_none:(w=false) k (clause:clause): clause option =
    let pattern, expr = clause in 
    match pattern with
    | Wildcard when w = false-> Some (Wildcard, expr)
    | Constructor (k', plist) when k' = k ->
      if (List.length plist > 1) then
        Some (Constructor (Tuple, plist), expr)
      else
        Some (plist |> List.hd, expr)
    | _ -> None
  in
  let negate_piop = function
    | EqInt i -> NeqInt i
    | NeqInt i -> EqInt i
    | IsK s -> NotK s
    | NotK s -> IsK s
    | IsTrue -> IsFalse
    | IsFalse -> IsTrue
    | IsTuple -> NotTuple
    | NotTuple -> IsTuple
    | IsNil -> NotNil
    | NotNil -> IsNil
    | IsCons -> NotCons
    | NotCons -> IsCons
    | EqString s -> NeqString s
    | NeqString s -> EqString s
  in
  let piop_of_constructor = function
    | Variant s -> IsK s
    | Int i -> EqInt i
    | Bool b -> if b then IsTrue else IsFalse
    | String s -> EqString s
    | Tuple -> IsTuple
    | Nil -> IsNil
    | Cons -> IsCons
  in
  let rec eval_clauses ?idx:(idx=0) constraints clauses : constraint_tree list =
    match clauses with
    | clause::tl -> let pattern, exp = clause
      in
      begin match pattern with
        | Or (p1, p2) ->
          eval_clauses constraints ((p1,exp)::(p2,exp)::tl) 
        | Constructor (k, plist) ->
          let pi = {idx=idx; op=piop_of_constructor k} in
          let notpi = {pi with op=negate_piop pi.op} in 
          let constraints' = (notpi :: constraints) in
          begin match k with
            | Int _ | String _ | Bool _ ->
              assert (plist = []);
              Leaf ([pi], exp)::(eval_clauses constraints' tl)
            | Cons | Tuple -> assert (plist <> []);
              let flat_children = plist |>
                                  List.mapi (fun i pattern -> eval_clauses ~idx:i [] [(pattern, exp)]) |>
                                  BatList.n_cartesian_product |>
                                  List.map (fun n -> Conjunction n)
              in
              flat_children @ eval_clauses constraints tl
            | Variant _ when plist <> [] ->
              let children = clauses |>
                          List.filter_map (shares_kst k) |>
                          eval_clauses [] in
              let brothers = clauses |>
                             List.filter (fun c ->
                                 shares_kst ~wildcard_to_none:true k c |> Option.is_none) |>
                             eval_clauses constraints'
              in
              Node (pi, children) :: brothers
            | Nil | Variant _ ->
              assert (plist = []);
              Leaf ([pi], exp) :: (eval_clauses constraints' tl)
          end
        | As (pattern, _) -> eval_clauses constraints ((pattern, exp)::tl)
        | Wildcard ->
          (* There could be a wildcard after another one, eg:
           *   | K2 true -> ...
           *   | K2 _ -> ...
           *   | _ -> ...
           * In that case we ignore it
          *)
          [Leaf (constraints, exp)]
      end
    | [] -> []
  in
  Root (eval_clauses [] source.clauses)

let eval source_ast =
  let result = sym_exec source_ast in
  print_result result

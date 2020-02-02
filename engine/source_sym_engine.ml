open Ast

type accessor =
  | AcRoot
  | AcField of accessor * int

type constraint_tree =
  | Unreachable
  | Failure
  | Leaf of source_expr
  | Node of accessor * (constructor * constraint_tree) list * constraint_tree
(* We distinguish
   - Unreachable: we statically know that no value can go there
   - Failure: a value matching this part results in an error

  If we had a type-declaration-based analysis to know the list of constructors
  at a given type, we could produce Unreachable instead of Failure for
  fallbacks of closed signature:

    (function true -> 1)

  returns in, morally

    Node ([(true, Leaf 1)], Failure)

  while

    (function true -> 1 | false -> 2)

  will (somday) give

    Node ([(true, Leaf 1); (false, Leaf 2)], Unreachable)

  In the meantime, it is possible to produce Unreachable examples by using
  OCaml refutation clauses (a "dot" in the right-hand-side)

    (function true -> 1 | false -> 2 | _ -> .)

  We trust this annotation, which is reasonable as the OCaml type-checker
  verifies that it indeed holds.
*)


let string_of_source_expr = function
  | SBlackbox s -> s

let print_result stree =
  let bprintf = Printf.bprintf
  in
  let bprint_source_expr buf = function
    | SBlackbox s -> bprintf buf "%s" s
  in
  let rec bprint_accessor buf = function
    | AcRoot -> bprintf buf "AcRoot"
    | AcField (a, i) -> bprintf buf "%a.%d" bprint_accessor a i
  in
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
    let bprint_constructor buf k = match k with
      | Variant s -> bprintf buf "Variant %s" s
      | Int i -> bprintf buf "Int %d" i
      | Bool b -> bprintf buf "Bool %b" b
      | String s -> bprintf buf "String \"%s\"" s
      | Tuple narity -> bprintf buf "Tuple[%d]" narity
      | Nil ->  bprintf buf "Nil"
      | Cons -> bprintf buf "Cons"
    in
    match tree with
    | Failure -> bprintf buf "Failure"
    | Unreachable -> bprintf buf "Unreachable"
    | Leaf expr ->
      bprintf buf
        "Leaf='%a'"
        bprint_source_expr expr
    | Node (ac, k_cst_list, fallback_cst) ->
      bprintf buf "Node %a:{\
                   %a \
                   %t} Fallback: %a"
        bprint_accessor ac
        (bprint_list ~sep:sep
           (fun buf (k,cst) -> bprintf buf "%t%a -> %t%a"
               sep
               bprint_constructor k
               (break (ntabs+2))
               (bprint_tree (ntabs+1)) cst))
        k_cst_list
        (break ntabs)
        (bprint_tree (ntabs+1)) fallback_cst
  in
  let buf = Buffer.create 42 in
  bprint_tree 0 buf stree;
  BatIO.write_line BatIO.stdout (Buffer.contents buf)

type row = pattern list * source_rhs
type matrix = accessor list * row list

type group = {
  arity: int;
  accessors: accessor list;
  rev_rows: row list ref;
}

let matrix_of_group { accessors; rev_rows; _ } : matrix =
  (accessors, List.rev !rev_rows)

let empty_group acs arity =
  match acs with
  | [] -> assert false
  | ac :: acs ->
     let accessors = List.init arity (fun i -> AcField(ac, i)) @ acs in
     {
       arity;
       accessors;
       rev_rows = ref [];
     }

let group_add_children { arity; rev_rows; _ } children (rest, rhs) =
  assert (List.length children = arity);
  rev_rows := (children @ rest, rhs) :: !rev_rows

let group_add_omegas { arity; rev_rows; _ } (rest, rhs) =
  let wildcards = List.init arity (fun _ -> (Wildcard : pattern)) in
  rev_rows := (List.rev_append wildcards rest, rhs) :: !rev_rows

let group_constructors (acs, rows) : (constructor * matrix) list * matrix =
  let group_tbl : (constructor, group) Hashtbl.t = Hashtbl.create 42 in
  let wildcard_group = empty_group acs 0 in
  let rec collect_constructors : pattern list -> unit = function
    | [] -> ()
    | (pattern::ptl) ->
      match pattern with
      | Wildcard -> ()
      | Or (p1, p2) -> collect_constructors (p1::p2::ptl)
      | As (p, _) -> collect_constructors (p::ptl)
      | Constructor (k, plist) ->
        if not (Hashtbl.mem group_tbl k) then begin
          let arity = List.length plist in
          Hashtbl.add group_tbl k (empty_group acs arity)
        end;
        collect_constructors ptl
  in
  List.iter (fun (pats, _) -> collect_constructors pats) rows;
  let all_constructor_groups =
    group_tbl |> Hashtbl.to_seq_values |> List.of_seq
  in
  let rec put_in_group : pattern list * source_rhs -> unit = function
    | ([], _expr) -> assert false
    | ((pattern::ptl), expr) ->
      let row_rest = (ptl, expr) in
      match pattern with
      | Constructor (k, plist) ->
        let group = Hashtbl.find group_tbl k in
        group_add_children group plist row_rest
      | Wildcard ->
        List.iter (fun group -> group_add_omegas group row_rest)
          (wildcard_group :: all_constructor_groups);
      | As (pattern, _) -> put_in_group (pattern::ptl, expr)
      | Or (p1, p2) -> put_in_group (p1::ptl, expr); put_in_group (p2::ptl, expr)
  in
  List.iter put_in_group rows;
  let constructor_matrices =
    group_tbl
    |> Hashtbl.to_seq
    |> Seq.map (fun (k, group) -> (k, matrix_of_group group))
    |> List.of_seq
  in
  let wildcard_matrix = matrix_of_group wildcard_group in
  (constructor_matrices, wildcard_matrix)

let sym_exec source =
  let rec decompose matrix : constraint_tree =
    match matrix with
    | (_, []) -> assert false
    | ([] as _no_acs, ([] as _no_columns, rhs)::_) ->
       begin match (rhs : source_rhs) with
         | Unreachable -> Unreachable
         | Expr expr -> Leaf expr
       end
    | (_::_ as _accs, ([] as _no_columns, _)::_) -> assert false
    | ([] as _no_accs, (_::_ as _columns, _)::_) -> assert false
    | (ac_head::_ as _acs, ((_::_) as _columns, _)::_) ->
      let groups, fallback = group_constructors matrix in
      let groups_evaluated =
        groups |> List.map (fun (k, submatrix) -> (k, decompose submatrix))
      in
      let fallback_evaluated = match fallback with
        | (_, []) -> Failure
        | nonempty_matrix -> decompose nonempty_matrix
      in
      Node (ac_head, groups_evaluated, fallback_evaluated)
    in
    let row_of_clause (pat, rhs) = ([pat], rhs) in
    decompose ([AcRoot], List.map row_of_clause source.clauses)

let eval source_ast =
  let result = sym_exec source_ast in
  print_result result

open Ast

module SMap = Map.Make(String)

type sym_value =
  | SAccessor of accessor
  | SCons of constructor * sym_value list

type constraint_tree =
  | Unreachable
  | Failure
  | Leaf of sym_value list
  | Guard of sym_value list * constraint_tree * constraint_tree
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

let print_result stree =
  let bprintf = Printf.bprintf
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
        (bprint_list ~sep bprint) xs
  in
  let bprint_constructor buf k = match k with
    | Variant s -> bprintf buf "Variant %s" s
    | Int i -> bprintf buf "Int %d" i
    | Bool b -> bprintf buf "Bool %b" b
    | String s -> bprintf buf "String \"%s\"" s
    | Unit -> bprintf buf "Unit"
    | Tuple narity -> bprintf buf "Tuple[%d]" narity
    | Nil ->  bprintf buf "Nil"
    | Cons -> bprintf buf "Cons"
  in
  let rec bprint_sym_value buf = function
    | SAccessor acc -> bprintf buf "%a"
                         bprint_accessor acc
    | SCons (k, svl) -> bprintf buf "%a %a"
                          bprint_constructor k
                          (bprint_list ~sep:ignore bprint_sym_value) svl
  in
  let break ntabs buf =
    bprintf buf "\n%s" (BatList.init ntabs (fun _ -> "\t") |> String.concat "") in
  let rec bprint_tree ntabs buf tree =
    let sep = break (ntabs+1) in
    match tree with
    | Failure -> bprintf buf "Failure"
    | Unreachable -> bprintf buf "Unreachable"
    | Leaf sym_value_list ->
      bprintf buf
        "Leaf='%a'"
        (bprint_list ~sep:ignore bprint_sym_value) sym_value_list
    | Guard (sym_value_list, ctrue, cfalse) ->
      let bprint_child prefix tree =
        bprintf buf
          "%t%s =%t%a"
          (break ntabs) prefix sep
          (bprint_tree (ntabs+1)) tree
      in
      bprintf buf "Guard (%a) ="
        (bprint_list ~sep:ignore bprint_sym_value) sym_value_list;
      bprint_child "guard(true)" ctrue ; bprint_child "guard(false)" cfalse
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

type matrix = accessor list * matrix_row list
and matrix_row = (pattern list * environment) row
and environment = sym_value SMap.t

type group = {
  arity: int;
  accessors: accessor list;
  rev_rows: matrix_row list ref;
  (* rev_rows: pattern list row list ref; *)
  }

let empty_env = SMap.empty

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

let width type_env = function
  | Nil | Cons -> 2
  | Bool _ -> 2
  | Unit | Tuple _ -> 1
  | Int _ | String _ -> max_int (* Just a way to indicate "many" *)
  | Variant v -> let type_decl = Source_env.ConstructorMap.find v type_env |> fst in
                 List.length type_decl.constructors

let group_add_children { arity; rev_rows; _ } children (row: matrix_row) =
  let lhs, env = row.lhs in
  assert (List.length children = arity);
  rev_rows := { row with lhs = ((children @ lhs), env) } :: !rev_rows

let group_add_omegas { arity; rev_rows; _ } (row: matrix_row) =
  let wildcards = List.init arity (fun _ -> (Wildcard : pattern)) in
  let (patterns, env) = row.lhs in
  rev_rows := { row with lhs = List.rev_append wildcards patterns, env } :: !rev_rows

let group_constructors type_env ((acs, rows) :matrix) : (constructor * matrix) list * matrix option =
  let group_tbl : (constructor, group) Hashtbl.t = Hashtbl.create 42 in
  let wildcard_group = empty_group acs 0 in
  let rec collect_constructors : pattern list -> unit = function
    | [] -> ()
    | (pattern::ptl) ->
      match pattern with
      | Wildcard -> ()
      | Or (p1, p2) -> collect_constructors (p1::p2::ptl)
      | As (p, _) -> collect_constructors (p::ptl)
      | Constructor (k, _plist) ->
        if not (Hashtbl.mem group_tbl k) then begin
          let arity = Source_env.constructor_arity type_env k in
          Hashtbl.add group_tbl k (empty_group acs arity)
        end;
        collect_constructors ptl
  in
  List.iter (fun row -> collect_constructors (fst row.lhs)) rows;
  let all_constructor_groups =
    group_tbl |> Hashtbl.to_seq_values |> List.of_seq
  in
  let rec put_in_group (row : matrix_row) =
    match row.lhs with
    | [], _ -> assert false
    | pattern::ptl, env ->
      let with_lhs (pats, env) = { row with lhs = (pats, env) } in
      let row_rest = with_lhs (ptl, env) in
      match pattern with
      | Constructor (k, plist) ->
        let group = Hashtbl.find group_tbl k in
        group_add_children group plist row_rest
      | Wildcard ->
        List.iter (fun group -> group_add_omegas group row_rest)
          (wildcard_group :: all_constructor_groups);
      | Or (p1, p2) ->
         put_in_group (with_lhs ((p1::ptl), env)); put_in_group (with_lhs ((p2::ptl), env))
      | As (pattern, var) ->
         let env' = SMap.add var (SAccessor (List.hd acs)) env in
         put_in_group (with_lhs ((pattern::ptl), env'))
  in
  List.iter put_in_group rows;
  let constructor_matrices =
    group_tbl
    |> Hashtbl.to_seq
    |> Seq.map (fun (k, group) -> (k, matrix_of_group group))
    |> List.of_seq
  in
  let width_of_column = List.length all_constructor_groups
  in
  let exhausted_all_cases = match BatHashtbl.to_list group_tbl with
    | [] -> false
    | (kst, _) :: _ -> width type_env kst = width_of_column
  in
  if not exhausted_all_cases then
    let wildcard_matrix = matrix_of_group wildcard_group in
    (constructor_matrices, Some wildcard_matrix)
  else
    (constructor_matrices, None)

let sym_exec type_env source =
  let rec source_value_to_sym_value env : source_value -> sym_value = function
    | VConstructor (k, svl) -> SCons (k, List.map (source_value_to_sym_value env) svl)
    | VVar v -> SMap.find v env
  in
  let rec decompose (matrix : matrix) : constraint_tree =
    match matrix with
    | (_, []) -> Failure
    | ([] as _no_acs, ({ lhs = ([], env); guard = None;_ } as row)::_) ->
       begin match (row.rhs : source_rhs) with
         | Unreachable -> Unreachable
         | Observe expr -> Leaf (List.map (source_value_to_sym_value env)expr)
       end
    | ([] as _no_acs,
       ({ lhs = ([], env); guard = Some (Guard guard); _ } as row) :: rest)
      ->
       Guard (List.map (source_value_to_sym_value env) guard,
              decompose ([], { row with guard = None } :: rest),
              decompose ([], rest))
    | (_::_ as _accs, { lhs = ([], _); _ }::_) -> assert false
    | ([] as _no_accs, { lhs = (_::_, _); _  }::_) -> assert false
    | (ac_head::_ as _acs, { lhs = (_::_, _); _ }::_) ->
      let groups, fallback = group_constructors type_env matrix in
      let groups_evaluated =
        groups |> List.map (fun (k, submatrix) -> (k, decompose submatrix))
      in
      let fallback_evaluated = match fallback with
        | None -> Unreachable
        | Some nonempty_matrix -> decompose nonempty_matrix
      in
      Node (ac_head, groups_evaluated, fallback_evaluated)
  in
  let row_of_clause clause = { clause with lhs = ([clause.lhs], empty_env) } in
  decompose ([AcRoot], List.map row_of_clause source.clauses)

(* alias of sym_exec, for consistency with Target_sym_engine *)
let eval type_env source_ast =
  sym_exec type_env source_ast

open Ast

type constraint_tree =
  | Empty
  | Leaf of source_expr
  | Node of (constructor * constraint_tree) list * constraint_tree

let string_of_source_expr = function
  | SBlackbox s -> s

let print_result stree =
  let bprintf = Printf.bprintf
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
    | Empty -> bprintf buf "Empty"
    | Leaf expr ->
      bprintf buf
        "Leaf='%a'"
        bprint_source_expr expr
    | Node (k_cst_list, fallback_cst) ->
      bprintf buf "Node:{\
                   %a \
                   %t} Fallback: %a"
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

type row = pattern list * source_expr

let sym_exec source =
  let group_constructors rows : (constructor * row list) list * row list =
    let grouptbl = Hashtbl.create 42
    in
    let (ksttbl: (constructor, int) Hashtbl.t) = Hashtbl.create 42
    in
    let wildcardgroup = ref []
    in
    let arity_of_k k =
      Hashtbl.find ksttbl k
    in
    let add_wildcards n (wlist, expr) : pattern list * source_expr =
      if n > 0 then
        (List.rev_append (List.init n (fun _: pattern -> Wildcard)) wlist, expr)
      else
        (wlist, expr)
    in
    let rec collect_constructors : pattern list -> unit = function
      | [] -> ()
      | (pattern::ptl) ->
        match pattern with
        | Wildcard -> ()
        | Or (p1, p2) -> collect_constructors (p1::p2::ptl)
        | As (p, _) -> collect_constructors (p::ptl)
        | Constructor (k, plist) ->
          let narity = List.length plist in
          Hashtbl.replace ksttbl k narity;
          collect_constructors ptl
    in
    let rec put_in_group : pattern list * source_expr -> unit = function
      | ([], _expr) -> assert false
      | ((pattern::ptl), expr) ->
        match pattern with
        | Constructor (k, plist) ->
          let binding = match Hashtbl.find_opt grouptbl k with
            | Some lst -> (plist@ptl, expr)::lst
            | None -> (plist@ptl, expr)::[]
          in
          Hashtbl.replace grouptbl k binding
        | Wildcard -> let wclause = (ptl, expr)
          in (
            wildcardgroup := wclause::!wildcardgroup;
            grouptbl |> BatHashtbl.map_inplace
              (fun k lst -> let n = (arity_of_k k - List.length ptl) in
                (add_wildcards n wclause)::lst)
          )
        | As (pattern, _) -> put_in_group (pattern::ptl, expr)
        | Or (p1, p2) -> put_in_group (p1::ptl, expr); put_in_group (p2::ptl, expr) 
    in
    List.iter (fun row -> fst row |> collect_constructors) rows;
    List.iter put_in_group rows;
    let fst = BatHashtbl.bindings grouptbl
            |> List.map (fun (k, ptl) -> (k, List.rev ptl))
    in
    let snd = !wildcardgroup
    in
    (fst, snd)
  in
  let rec decompose (rows: row list) : constraint_tree =
    match rows with
    | [] -> assert false
    | ([], expr)::[] -> Leaf expr
    | ([], expr)::tl -> ignore tl; Leaf expr
    | ((_pattern::_ptl), _expr)::_tl ->
      let groups, fallback = group_constructors rows
      in
      let groups_evaluated =  groups
                              |> List.map (fun (k, clause_lst) ->
                                  k, decompose clause_lst)
      in
      let fallback_evaluated = match fallback with
        | [] -> Empty
        | clause -> decompose clause
      in
      Node (groups_evaluated, fallback_evaluated)
      (* TODO: DISCUSS: 
       * Sometime groups_evaluated = []; Do we want to express that better? *)
    in
    source.clauses |> List.map (fun (pattern, expr) -> ([pattern], expr)) |> decompose

let eval source_ast =
  let result = sym_exec source_ast in
  print_result result

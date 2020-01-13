open Ast

type constraint_tree =
  | Leaf of source_expr
  | Node of (constructor * constraint_tree) list * constraint_tree option

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
      | String s -> bprintf buf "String %s" s
      | Tuple narity -> bprintf buf "Tuple[%d]" narity
      | Nil ->  bprintf buf "Nil"
      | Cons -> bprintf buf "Cons"
    in
    match tree with
    | Leaf expr ->
      bprintf buf
        "Leaf='%a'"
        bprint_source_expr expr
    | Node (k_cst_list, cst_opt) ->
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
        (fun buf -> begin match cst_opt with 
             | Some cst -> fun _ -> bprint_tree (ntabs+1) buf cst
             | None -> fun _ -> bprintf buf "None" 
           end) buf
  in
  let buf = Buffer.create 42 in
  bprint_tree 0 buf stree;
  BatIO.write_line BatIO.stdout (Buffer.contents buf)

type row = pattern list * source_expr

let sym_exec source =
  let kst_rows_assoc rows : (constructor * row list) list * row option =
    let hashtbl = Hashtbl.create 42
    in
    let with_wildcard = ref []
    in
    let relevant_wildcard_opt = function
      | [] -> None
      | wildcards -> Some (wildcards |> List.rev |> List.hd)
    in
    let map_to_hashtable = 
      fun ((pattern, expr):row) ->
        match List.hd pattern with
        | Constructor (k, plist) ->
          let binding = match Hashtbl.find_opt hashtbl k with
            | Some clause_lst -> (plist, expr)::clause_lst
            | None -> (plist, expr)::[]
          in
          Hashtbl.replace hashtbl k binding
        | Wildcard -> with_wildcard := (pattern, expr)::!with_wildcard
        | _ -> ()
    in
    List.iter map_to_hashtable rows;
    let fst = BatHashtbl.bindings hashtbl
              |> List.map (fun (k, rows) ->
                  (k, List.rev (!with_wildcard@rows)))
    in
    let snd = relevant_wildcard_opt !with_wildcard
    in
    (fst, snd)
  in
  let rec decompose (rows: row list) : constraint_tree =
    match rows with
    | ((pattern::ptl), expr)::tl ->
      begin match pattern with
        | Or (p1, p2) ->
          decompose (((p1::ptl), expr) :: ((p2::ptl), expr) :: tl)
        | Constructor (_, _) ->
          let split, fallback = kst_rows_assoc rows
          in
          let split_evaluated =  split
                                 |> List.map (fun (k, clause_lst) ->
                                     k, decompose clause_lst)
          in
          let fallback_evaluated = match fallback with
            | Some clause -> Some (decompose [clause])
            | None -> None
          in
          Node (split_evaluated, fallback_evaluated)
        | As (pattern, _) -> decompose (([pattern], expr)::tl)
        | Wildcard -> ignore tl;
          Leaf expr
      end
    | [] -> assert false
    | ([], expr)::[] -> Leaf expr
    | ([], expr)::tl -> ignore tl; Leaf expr (* TODO: DISCUSS: is it correct to ignore tail? *)
  in
  source.clauses |> List.map (fun (pattern, expr) -> ([pattern], expr)) |> decompose


let eval source_ast =
  let result = sym_exec source_ast in
  print_result result

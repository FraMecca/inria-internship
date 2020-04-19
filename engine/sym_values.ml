open Ast
open Merge_accessors

let print str = BatIO.write_string BatIO.stdout ("\tSym_values: "^str^"\n")

module Domain = Target_sym_engine.Domain

type source_sym_values = Source_sym_engine.sym_value list
type target_sym_values = Merge_accessors.sym_value list

type source_canonical_form =
  | Int of int
  | NonSingleton of accessor
  | Block of int * source_canonical_form list

type target_canonical_form =
  | Int of int
  | NonSingleton of accessor
  | Block of int * target_canonical_form list

let compare_sym_value find_constructor_of find_domain_of (src, tgt): bool =
  let rec canonical_form_of_source_sym_value : Source_sym_engine.sym_value -> source_canonical_form = function
    | SAccessor acc ->
      let pi = find_domain_of acc in
      if Domain.is_int_singleton pi then
        Int (Domain.get_int_singleton pi)
      else
        NonSingleton acc
    | SCons (constructor, rest) ->
      let rest' = List.map canonical_form_of_source_sym_value rest in
      match constructor with
      | Int i -> assert (rest' = []); Int i
      | Bool false -> assert (rest' = []); Int 0
      | Bool true -> assert (rest' = []); Int 1
      | String _ -> failwith "not implemented"
      | Nil -> assert (rest' = []); Int 0
      | Cons | Tuple _ -> Block (0, rest')
      | Variant v -> print ("%%%%%%%%%%%%%%%%"^v);
        let open Source_env in
        match (fun v : Source_env.constructor_repr -> find_constructor_of v) v with
        | Int i -> assert (rest' = []); Int i
        | Tag t -> Block (t, rest')
  in
  let rec canonical_form_of_target_sym_value : Merge_accessors.sym_value -> target_canonical_form = function
    | VConstant i -> Int i
    | VConstructor {tag=t; args=rest} ->
      Block (t, List.map canonical_form_of_target_sym_value rest)
    | VAccessor acc ->
      let pi = find_domain_of acc in
      if Domain.is_int_singleton pi then
        Int (Domain.get_int_singleton pi)
      else
        (* domain is ignored because the value at runtime
           can be instantiated with several different values *)
        NonSingleton acc
  in
  let rec compare_canonical_form_ : source_canonical_form * target_canonical_form -> bool = function
    | (NonSingleton s, NonSingleton t) -> print ("NS-NS = "^string_of_bool (s=t));  s = t 
    | (Int s, Int t) -> print ("I-I: "^string_of_int s^":"^string_of_int t^" = "^string_of_bool (s=t)); s = t
    | (Block (s, rest), Block (t, rest')) -> print ("B-B = "^string_of_bool (s=t)); s = t &&
                                             List.combine rest rest'
                                             |> List.for_all compare_canonical_form_
    | (s, t) ->
       let source_st = match s with
         | Int i -> "Int "^string_of_int i
         | NonSingleton _a -> "NS "
         | Block (i, _) -> "B"^string_of_int i in
       let target_st = match t with
         | Int i -> "Int "^string_of_int i
         | NonSingleton _a -> "NS "
         | Block (i, _) -> "B"^string_of_int i in
       print ("Failing sym_eq with: "^source_st^" - "^target_st) ;
       false
  in
  compare_canonical_form_ ((canonical_form_of_source_sym_value src), (canonical_form_of_target_sym_value tgt))

let string_of_tvl (sv: target_sym_values) = 
  let bprintf = Printf.bprintf in
  let comma buf = bprintf buf ", " in
  let rec bprint_list ~sep bprint buf = function
    | [] -> ()
    | [x] -> bprint buf x
    | x :: xs ->
       bprintf buf "%a%t%a"
         bprint x
         sep
         (bprint_list ~sep bprint) xs in
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
  let buf = Buffer.create 42 in
  bprintf buf "%a" (bprint_list ~sep:comma bprint_sym_value) sv;
  Buffer.contents buf

let string_of_svl (sv: source_sym_values) =
  let open Source_sym_engine in
  let bprintf = Printf.bprintf in
  let comma buf = bprintf buf ", " in
  let rec bprint_list ~sep bprint buf = function
    | [] -> ()
    | [x] -> bprint buf x
    | x :: xs ->
       bprintf buf "%a%t%a"
         bprint x
         sep
         (bprint_list ~sep bprint) xs in
  let rec bprint_accessor buf = function
    | AcRoot -> bprintf buf "AcRoot" 
    | AcField (a, i) -> bprintf buf "AcFiled %a.%d" bprint_accessor a i
  in
  let bprint_constructor buf k = match k with
    | Variant s -> bprintf buf "Variant %s" s
    | Int i -> bprintf buf "Int %d" i
    | Bool b -> bprintf buf "Bool %b" b
    | String s -> bprintf buf "String \"%s\"" s
    | Tuple narity -> bprintf buf "Tuple[%d]" narity
    | Nil ->  bprintf buf "Nil"
    | Cons -> bprintf buf "Cons"
  in
  let rec bprint_sym_value buf = function
    | SAccessor acc -> bprintf buf "%a"
                         bprint_accessor acc
    | SCons (k, svl) -> bprintf buf "Cons{k=%a; args=%a}"
                          bprint_constructor k
                          (bprint_list ~sep:comma bprint_sym_value) svl
  in
  let buf = Buffer.create 42 in
  bprintf buf "%a" (bprint_list ~sep:comma bprint_sym_value) sv;
  Buffer.contents buf

let compare_sym_values find_constructor_of find_accessor_of sym_values target_values =
  print "!!!!!!!!!!!!!!!";
  print (string_of_tvl target_values ^ ": "^ (target_values |> List.length |> string_of_int));
  print (string_of_svl sym_values ^ ": "^ (sym_values |> List.length |> string_of_int));
  print "!!!!!!!!!!!!!!!";
  if List.length target_values <> List.length sym_values then
    (print "False for diff length"; false)
  else 
    List.combine sym_values target_values
    |> List.for_all (compare_sym_value find_constructor_of find_accessor_of)

open Merge_accessors
open Ast

let print str = BatIO.write_string BatIO.stdout ("- EQ: "^str^"\n")

module Domain = Target_sym_engine.Domain

module AcMap = Map.Make(
  struct type t = accessor
    let rec compare a a' =
      match (a, a') with
      | (AcRoot, AcRoot) -> 0
      | (AcRoot, AcField _) -> -1
      | (AcField _, AcRoot) -> 1
      | (AcField (a, i), AcField (a', i')) when a = a' -> Int.compare i i'
      | (AcField (a, _), AcField (a', _)) -> compare a a'
  end)

type source_tree = Source_sym_engine.constraint_tree
type target_tree = Merge_accessors.constraint_tree
type source_sym_values = Sym_values.source_sym_values
type target_sym_values = Sym_values.target_sym_values

type source_constructor = Ast.constructor

let variant_name_to_domain repr_env v_name =
  let open Source_env in
  match ConstructorMap.find v_name repr_env with
  | Int i -> Domain.int (Domain.Set.point i)
  | Tag t -> Domain.tag (Domain.Set.point t)

let constructor_to_domain repr_env : constructor -> domain = function
  | Int i -> Domain.int (Domain.Set.point i)
  | Bool false -> Domain.int (Domain.Set.point 0)
  | Bool true -> Domain.int (Domain.Set.point 1)
  | String _ -> failwith "not implemented"
  | Nil -> Domain.int (Domain.Set.point 0)
  | Cons | Tuple _ -> Domain.tag (Domain.Set.point 0)
  | Variant v -> variant_name_to_domain repr_env v

let constrained_subtrees type_env repr_env children fallback =
  let head_domain kst_list =
    let _head_domain : constructor -> Domain.t = function
      | String _ -> Domain.tag (Domain.Set.point Obj.string_tag)
      | Bool _ -> Domain.union (Domain.int (Domain.Set.point 0)) (Domain.int (Domain.Set.point 1))
      | Int _ -> Domain.int (Domain.Set.full)
      | Tuple _ -> Domain.tag (Domain.Set.point 0)
      | Nil | Cons -> Domain.union (Domain.int (Domain.Set.point 0)) (Domain.tag (Domain.Set.point 0))
      | Variant v ->
         let open Source_env in
         let type_decl = ConstructorMap.find v type_env |> fst in
         let kst_list = type_decl.constructors in
         List.map (fun k -> variant_name_to_domain repr_env k.constructor_name) kst_list
         |> List.fold_left Domain.union Domain.empty
    in
    List.map _head_domain kst_list |> List.fold_left Domain.union Domain.empty
  in
  let children' = List.map (fun (kst, tree) ->
                      (constructor_to_domain repr_env kst, tree)) children in
  let fb_domain =
    children'
    |> List.map fst
    |> List.fold_left Domain.union Domain.empty
    |> Domain.negate
    |> Domain.inter (List.map fst children |> head_domain)
  in
  ( print "CONSTRAINED:";
    (* List.map snd children' |> List.iter (fun t -> print "------>";Source_sym_engine.print_result t); *)
    print ("DOMCH: "^(children'|> List.map fst|> List.fold_left Domain.union Domain.empty|>Domain.to_string));
    print ("FALLBACK: "^Domain.to_string fb_domain);
    Source_sym_engine.print_result fallback;
    print "END CONSTRAINED";
  ((fb_domain, fallback) :: children')
  )

let compare (type_env) (repr_env: Source_env.type_repr_env) (left: source_tree) (right: target_tree) : bool =
  let rec trim src_acc src_pi =
    let specialize_same_acc node_acc (dom, s_tree) =
      let dom' =
        if src_acc <> node_acc then dom
        else Domain.inter src_pi dom in
      if Domain.is_empty dom' then None
      else (
        print ("Trimmed: SRC="^Merge_accessors.accessor_to_string src_acc^
                 ":"^Domain.to_string src_pi^" |⋂| "^
                   "TRG="^Merge_accessors.accessor_to_string node_acc^":"^Domain.to_string dom^
                     " |TO| "^Domain.to_string dom');
        Some (dom', trim src_acc src_pi s_tree)
      )
    in
    function
    | Node (_, [], None) -> failwith "Shouldn't happen: Node with no branches and no fallback case"
    | Failure -> Failure
    | Leaf l -> Leaf l
    | Guard (bb, ctrue, cfalse) -> Guard (bb, ctrue, cfalse)
    | Node (node_acc, children, fallback) ->
       let children' =
         children
         |> List.filter_map (specialize_same_acc node_acc)
       in
       let fallback' =
         Option.bind fallback (specialize_same_acc node_acc)
       in
       Node (node_acc, children', fallback')
  in
  let specialize_input_space acc pi input_space =
    print ("Specialize_input_space: "^Merge_accessors.accessor_to_string acc^" -> "^Domain.to_string pi);
    let pi' = match AcMap.find_opt acc input_space with
      | Some input_pi -> Domain.inter input_pi pi
      | None -> pi
    in
    AcMap.add acc pi' input_space
  in
  let dead_end input_space =
    AcMap.exists (fun _ value -> Domain.is_empty value) input_space
  in
  let rec compare_ cnt type_env (input_space: domain AcMap.t) (guards: (source_sym_values * bool) list) (left: source_tree) (right: target_tree) : bool =
    let sym_values_eq = Sym_values.compare_sym_values
                          (fun variant_name -> Source_env.ConstructorMap.find variant_name repr_env)
                          (fun acc -> AcMap.find acc input_space)
    in
    print ("================"^(string_of_int cnt)^"=====================");
    print "Input space";
    AcMap.iter (fun a d -> print ("\t"^Merge_accessors.accessor_to_string a^": "^Domain.to_string d)) input_space;
    Source_sym_engine.print_result left;
    Merge_accessors.print_tree right;
    if dead_end input_space then (print "Dead_end";
      true)
    else
      match (left, right) with
      | Node (acc, children, fallback), _ ->  print "Node-Node";
         let compare_branch (pi, branch) =
           let input_space' = specialize_input_space acc pi input_space in
           compare_ (cnt+1) type_env input_space' guards branch (trim acc pi right)
         in
         constrained_subtrees type_env repr_env children fallback
         |> List.for_all compare_branch
      | ((Failure | Leaf _) as terminal, Node (_, children, fallback)) -> print "lf-Node";
         Option.to_list fallback @ children
         |> List.for_all (fun (_, child) -> compare_ (cnt+1) type_env input_space guards terminal child)
      | (Unreachable, _) -> print "match unreachable";
         true 
      | (Guard (svl, ctrue, cfalse), _) -> print ("+++ push guard | "^ Sym_values.string_of_svl svl);
         compare_ (cnt+1) type_env input_space (guards@[(svl, true)]) ctrue right &&
         compare_ (cnt+1) type_env input_space (guards@[(svl, false)]) cfalse right
      | (_, Guard (tvl, ctrue, cfalse)) -> print ("--- pop guard | "^ Sym_values.string_of_tvl tvl);
         begin match guards with
         | hd::grest when sym_values_eq (fst hd) tvl ->
            if snd hd then
              compare_ (cnt+1) type_env input_space grest left ctrue
            else
              compare_ (cnt+1) type_env input_space grest left cfalse
         | h::_ -> print "false with guards.len > 0:";
                   print ("@@@  Source: | "^ Sym_values.string_of_svl (fst h));
                   print ("@@@  Target: | "^ Sym_values.string_of_tvl tvl);
                   print (sym_values_eq (fst h) tvl |> string_of_bool);
                   false
         | _ -> false
         end
      | (Failure, Failure) -> print "failure-failure"; guards = []
      | (Leaf slf, Leaf rlf) -> print "Leaf-Leaf"; guards = [] && sym_values_eq slf rlf
      | (Failure, Leaf _) | (Leaf _, Failure) -> print "LF-FL";
        false
  in
  compare_ 0 type_env AcMap.empty [] left right

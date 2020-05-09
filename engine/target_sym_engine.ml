open Ast

(* TODO: change naming to decision tree *)

let target_example = {|(let 
                        (r/1204 =
                            (function param/1206
                                (if (!= param/1206 1) (if (!= param/1206 2) "0" "2") "1"))))|}

module SMap = Map.Make(String)
module IMap = Map.Make(struct type t = int let compare = compare end)
module Domain = Target_domain

type constraint_tree =
  | Failure
  | Leaf of sym_value list
  | Node of accessor * (domain * constraint_tree) list * (domain * constraint_tree) option
  | Guard of sym_value list * constraint_tree * constraint_tree
and
  sym_value =
  | VConstructor of {tag:int; args:sym_value list}
  | VAccessor of accessor
  | VConstant of int
and
  pi = { var: accessor; domain: domain } (* record of a variable and a constraint on that variable *)
and
  sym_function = variable * constraint_tree
and
  sym_catch = exitpoint * variable list * constraint_tree
and
  environment = {
  values: accessor SMap.t;
  functions: sym_function SMap.t;
  exits: sym_catch IMap.t;
}
and
  accessor =
  | AcRoot of variable
  | AcField of accessor * int
  | AcAdd of accessor * int
and
  domain = Domain.t

let print_tree tree =
  let bprintf = Printf.bprintf
  in
  let rec bprint_svalue buf = function
    | AcRoot (v) -> bprintf buf "AcRoot=%s" v
    | AcField (a, i) -> bprintf buf "AcField(%d %a)" i bprint_svalue a
    | AcAdd (a, i) -> bprintf buf "AcAdd(%d %a)" i bprint_svalue a
  in
  let bprint_pi buf pi =
    bprintf buf "{ var=%a; dom=%a; }" bprint_svalue pi.var Domain.bprint pi.domain
  in
  let rec bprint_list ~sep bprint buf = function
    | [] -> ()
    | [x] -> bprint buf x
    | x :: xs ->
      bprintf buf "%a%t%a"
        bprint x
        sep
        (bprint_list ~sep bprint) xs in
  let indent ntabs buf =
    bprintf buf "%s" (List.init ntabs (fun _ -> "\t") |> String.concat "")
  in
  let _break ntabs buf =
    bprintf buf "\n%t" (indent ntabs)
  in
  let comma buf = bprintf buf ", " in
  let rec bprint_accessor buf = function
    | AcRoot v -> bprintf buf "AcRoot %s" v
    | AcField (a, i) -> bprintf buf "AcField %a.%d" bprint_accessor a i
    | AcAdd (a, i) -> bprintf buf "AcAdd %a.%d" bprint_accessor a i
  in
  let rec bprint_sym_value buf = function
    | VAccessor acc -> bprintf buf "VAccessor:%a" bprint_accessor acc
    | VConstant i -> bprintf buf "VConstant:%d" i
    | VConstructor {tag=t; args=a} -> bprintf buf "VConstructor:{tag=%d; args=%a}"
                                        t
                                        (bprint_list ~sep:comma bprint_sym_value) a
  in
  let rec bprint_tree ntabs buf tree =
    match tree with
    | Failure ->
      bprintf buf "%tFailure" (indent ntabs)
    | Leaf observe ->
      bprintf buf "%tLeaf=%a\n" (indent ntabs)
        (bprint_list ~sep:comma bprint_sym_value) observe
    | Guard (tgt_values, ctrue, cfalse) ->
      let bprint_child prefix tree =
        bprintf buf
          "%t%s =\n%a"
          (indent ntabs)
          prefix
          (bprint_tree (ntabs+1)) tree
      in
      bprintf buf "%tGuard (%a):\n" (indent ntabs)
        (bprint_list ~sep:comma bprint_sym_value) tgt_values;
      bprint_child "guard(true)" ctrue;
      bprint_child "guard(false)" cfalse
    | Node (var, children, fallback) ->
      let bprint_child buf (domain, tree) =
        bprintf buf
          "%tNode (%a) =\n%a"
          (indent ntabs)
          bprint_pi { var; domain }
          (bprint_tree (ntabs+1)) tree
      in
      bprint_list ~sep:ignore bprint_child buf children;
      match fallback with
      | Some (domain, tree) ->
        bprintf buf "%tFallback=Node (%a) =\n%a"
          (indent ntabs)
          bprint_pi {var; domain}
          (bprint_tree (ntabs+1)) tree
      | None -> bprintf buf "%tFallback=None\n" (indent ntabs)
  in
  let buf = Buffer.create 42 in
  bprint_tree 0 buf tree;
  BatIO.write_line BatIO.stdout (Buffer.contents buf)


let rec subst_svalue bindings = function
  | AcRoot v -> begin
      match List.assoc_opt v bindings with
        | Some svalue -> svalue
        | None -> AcRoot v
    end
  | AcField (acc', i) -> AcField (subst_svalue bindings acc', i)
  | AcAdd (svalue', i) -> AcAdd (subst_svalue bindings svalue', i)

let rec subst_tree bindings = function
  | Failure -> Failure
  | Leaf result -> Leaf result
  | Guard (bb, ctrue, cfalse) -> Guard (bb, ctrue, cfalse)
  | Node (var, children, fallback) ->
     let subst (dom, tree) =
       (dom, subst_tree bindings tree)
     in
     Node (subst_svalue bindings var,
           List.map subst children,
           Option.map subst fallback)


let eval target_ast =
  let empty_environment =
    { values=SMap.empty; functions=SMap.empty; exits=IMap.empty; }
  in
  let rec eval_sym_value env : Ast.target_value -> sym_value = function
    | VConstant i ->
      VConstant i
    | VConstructor {tag=t; args} ->
      let args' = List.map (eval_sym_value env) args in
      VConstructor {tag=t; args=args'}
    | VVariable var ->
      let acc = SMap.find var env.values in
      VAccessor acc
  in
  let rec sym_exec sexpr env : constraint_tree =
    let eval_bop (bop, i) = match bop with
      | Ge -> Domain.(int (Set.ge i))
      | Gt -> Domain.(int (Set.gt i))
      | Le -> Domain.(int (Set.le i))
      | Lt -> Domain.(int (Set.lt i))
      | Eq -> Domain.(int (Set.point i))
      | Nq -> Domain.(negate (int (Set.point i)))
    in
    let eval_switch_test: switch_test -> domain = function
      | Tag i -> Domain.(tag (Set.point i))
      | Int i -> Domain.(int (Set.point i))
    in
    let put_function variable fn : environment =
      assert (not (SMap.mem variable env.functions));
      {env with functions = SMap.add variable fn env.functions }
    in
    let put_exit variable ext : environment =
      assert (not (IMap.mem variable env.exits));
      {env with exits = IMap.add variable ext env.exits }
    in
    let put_value variable value : environment =
      assert (not (SMap.mem variable env.values));
      {env with values = SMap.add variable value env.values }
    in
    (* perform union on two maps, keys should never differ *)
    let union env1 env2 = {
      values=SMap.union (fun _ a b -> assert (a = b); Some a) env1.values env2.values;
      functions=SMap.union (fun _ a b -> assert (a = b); Some a) env1.functions env2.functions;
      exits=IMap.union (fun _ a b -> assert (a = b); Some a) env1.exits env2.exits;
    }
    in
    let find_var env : sexpr -> accessor = function
      | Var v -> SMap.find v env.values
      | _ -> assert false
    in
    let eval_let_binding env key (sxp : sexpr) =
      match sxp with
      | Var v ->
        put_value key (AcRoot v)
      | Field (i, v) ->
        let acc = SMap.find v env.values in
        put_value key (AcField (acc, i))
      | Addition (i, v) ->
        let svalue = SMap.find v env.values in
        put_value key (AcAdd (svalue, i))
      | Function (v, sxp) ->
        let envf = put_value v (AcRoot v) in
        let c_tree = sym_exec sxp envf in
        put_function key (v, c_tree)
      | _ -> assert false
    in
    match sexpr with
    | Let (blist, next_sexpr) ->
      let add_binding env' (var, sxp) =
        union env' (eval_let_binding env' var sxp)
      in
      let env' =
        List.fold_left add_binding env blist
      in
      sym_exec next_sexpr env'
    | If (bexpr, strue, sfalse) ->
      let test, sxp =
        match bexpr with
        | Comparison (bop, sxp, i) -> (eval_bop (bop, i)), sxp
        | Isout (i, v) -> Domain.isout i, Var v
        | Var v -> Domain.isnot 0, Var v
        | _ -> assert false
      in
      let var = find_var env sxp in
      Node (var, [
          (test, sym_exec strue env);
          (Domain.negate test, sym_exec sfalse env);
        ], None)
    | Switch (sxp, swlist, defcase) ->
      let var = find_var env sxp in
      let cases =
        List.map (fun (test, sxp) -> (eval_switch_test test, sxp)) swlist
      in
      let not_any_case =
        cases
        |> List.map (fun (dom, _) -> Domain.negate dom)
        |> List.fold_left Domain.inter Domain.full
      in
      let children = List.map (fun (dom, sxp) -> (dom, sym_exec sxp env)) cases in
      let fallback = match defcase with
        | Some tree -> Some (not_any_case, sym_exec tree env)
        | None -> None
      in
      Node (var, children, fallback)
    | Catch (sxp, extpt, varlist, exit_sxp) ->
      let c_tree = sym_exec exit_sxp env in
      let env' = put_exit extpt (extpt, varlist, c_tree) in
      sym_exec sxp env'
    | Exit (ext, sxps) ->
      let values = List.map (find_var env) sxps in
      let (ext', vars, c_tree) = IMap.find ext env.exits in
      assert (ext' = ext);
      assert (List.length vars = List.length values);
      let bindings = List.combine vars values in
      subst_tree bindings c_tree
    | String _ ->
      failwith "Not implemented"
    | Int n ->
      Leaf [VConstant n]
    | TBlackbox bb ->
      Leaf (List.map (eval_sym_value env) bb)
    | Match_failure ->
      Failure
    | Function (v, sxp) ->
      let envf = put_value v (AcRoot v) in
      sym_exec sxp envf
    | IfGuard (bb, strue, sfalse) ->
      Guard (List.map (eval_sym_value env) bb, sym_exec strue env, sym_exec sfalse env)
    | _ -> assert false
  in
  sym_exec target_ast empty_environment

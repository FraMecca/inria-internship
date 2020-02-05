open Ast

let target_example = {|(let 
                        (r/1204 =
                            (function param/1206
                                (if (!= param/1206 1) (if (!= param/1206 2) "0" "2") "1"))))|}

module SMap = Map.Make(String)
module IMap = Map.Make(struct type t = int let compare = compare end)

type constraint_tree =
  | Failure
  | Leaf of target_blackbox
  | Node of (pi * constraint_tree) list * (pi list * constraint_tree) option
and
  pi = { var: sym_value; op: piop } (* record of a variable and a constraint on that variable *)
and
  sym_function = variable * constraint_tree
and
  sym_catch = exitpoint * variable list * constraint_tree
and
  environment = {
  values: sym_value SMap.t;
  functions: sym_function SMap.t;
  exits: sym_catch IMap.t;
}
and
  sym_value =
  | AcRoot of variable
  | AcField of sym_value * int
  | AcTag of sym_value * int
  | AcAdd of sym_value * int
and
  piop =
  | Tag of int
  | NotTag of int (* Lambda doesn't have this *)
  | Int of int
  | NotInt of int (* Lambda doesn't have this *)
  | Ge of int
  | Gt of int
  | Le of int
  | Lt of int
  | Eq of int
  | Nq of int
  | Isout of int
  | Isin of int (* Lambda doesn't have this *)

let print_env env =
  let bprintf = Printf.bprintf
  in
  let rec bprint_svalue buf = function
    | AcRoot (v) -> bprintf buf "AcRoot=%s" v
    | AcField (a, i) -> bprintf buf "AcField(%d %a)" i bprint_svalue a
    | AcTag (a, i) -> bprintf buf "AcTag(%d %a)" i bprint_svalue a
    | AcAdd (a, i) -> bprintf buf "AcAdd(%d %a)" i bprint_svalue a
  in
  let bprint_pi buf pi =
    let print_op buf = function
      | Tag i -> bprintf buf "Tag %d" i
      | NotTag i -> bprintf buf "NotTag %d" i
      | Int i-> bprintf buf "Int %d" i
      | NotInt i-> bprintf buf "NotInt %d" i
      | Ge i -> bprintf buf "Ge %d" i
      | Gt i -> bprintf buf "Gt %d" i
      | Le i -> bprintf buf "Le %d" i
      | Lt i -> bprintf buf "Lt %d" i
      | Eq i -> bprintf buf "Eq %d" i
      | Nq i -> bprintf buf "Nq %d" i
      | Isout i -> bprintf buf "Isout %d" i
      | Isin i -> bprintf buf "Isin %d" i
    in
    bprintf buf "{ var=%a; op=%a; }" bprint_svalue pi.var print_op pi.op
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
  let break ntabs buf =
    bprintf buf "\n%t" (indent ntabs)
  in
  let rec bprint_tree ntabs buf tree =
    match tree with
    | Failure ->
       bprintf buf "%tFailure" (indent ntabs)
    | Leaf target_blackbox ->
       bprintf buf "%tLeaf=%S\n" (indent ntabs) target_blackbox
    | Node (children, fallback) ->
       let bprint_child buf (pi, tree) =
         bprintf buf
           "%tNode (%a) =\n%a"
           (indent ntabs)
           bprint_pi pi
           (bprint_tree (ntabs+1)) tree
       in
       bprint_list ~sep:ignore bprint_child buf children;
       match fallback with
       | Some (pis, tree) ->
         bprintf buf "%tFallback=Node (%a) =\n%a"
           (indent ntabs)
           (bprint_list ~sep:ignore bprint_pi) pis
           (bprint_tree (ntabs+1)) tree
       | None -> bprintf buf "%tFallback=None\n" (indent ntabs)
  and
  bprint_env ntabs buf env =
    let bprint_function buf binding =
      let (k, fn) = binding in
        let (v, f_tree) = fn
        in
        bprintf buf "%s: Function=%s,ConstraintTree:%t%a"
          k v
        (break ntabs)
        (bprint_tree (ntabs + 1)) f_tree
      in
      let bprint_exits buf binding =
      let (k, catch) = binding in
        let (e, vars, c_tree) = catch
        in
        bprintf buf "%d: Catch=%d %s,ConstraintTree:%t%a"
          k e
          (String.concat " " vars)
          (break ntabs)
          (bprint_tree (ntabs + 1)) c_tree
    in
    let bprint_svalues buf binding =
      let (k, svalue) = binding in
        bprintf buf "%s: %a" k bprint_svalue svalue
      in
    bprint_list ~sep:(break ntabs) bprint_function buf (SMap.bindings env.functions);
    bprint_list ~sep:(break ntabs) bprint_exits buf (IMap.bindings env.exits);
    bprint_list ~sep:(break ntabs) bprint_svalues buf (SMap.bindings env.values)
  in
    let buf = Buffer.create 42 in
    bprint_env 0 buf env;
    BatIO.write_line BatIO.stdout (Buffer.contents buf)


let rec subst_svalue bindings = function
  | AcRoot v -> begin
      match List.assoc_opt v bindings with
        | Some svalue -> svalue
        | None -> AcRoot v
    end
  | AcField (acc', i) -> AcField (subst_svalue bindings acc', i)
  | AcTag (acc', i) -> AcTag (subst_svalue bindings acc', i)
  | AcAdd (svalue', i) -> AcAdd (subst_svalue bindings svalue', i)

let rec subst_tree bindings = function
  | Failure -> Failure
  | Leaf result -> Leaf result
  | Node (children, fallback) ->
     let subst_pi bindings {var; op} =
       {var = subst_svalue bindings var; op}
     in
     let subst (pi, tree) =
       (subst_pi bindings pi,
        subst_tree bindings tree)
     in
     let fallback' = fallback
                     |> Option.map (fun (pis, tree) ->
                         ((List.map (subst_pi bindings) pis), subst_tree bindings tree))
     in
     Node ((List.map subst children), fallback')

let rec sym_exec sexpr env : constraint_tree =
  let match_bop: bop * int -> piop = function
    | Ge, i -> Ge i
    | Gt, i -> Gt i
    | Le, i -> Le i
    | Lt, i -> Lt i
    | Eq, i -> Eq i
    | Nq, i -> Eq i
  in
  let match_switch: switch_test -> piop = function
    | Tag i -> Tag i
    | Int i -> Int i
  in
  let negate = function
    | Tag i -> NotTag i
    | NotTag i -> Tag i
    | Int i -> NotInt i
    | NotInt i -> Int i
    | Ge i -> Lt i
    | Gt i -> Le i
    | Le i -> Gt i
    | Lt i -> Ge i
    | Eq i -> Nq i
    | Nq i -> Eq i
    | Isout i -> Isin i
    | Isin i -> Isout i
  in
  let negate_pi {var; op} = {var; op = negate op} in
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
  let find_var env : sexpr -> sym_value = function
    | Var v -> SMap.find v env.values
    | _ -> assert false
  in
  let eval_let_binding env (sxp : sexpr) key =
    match sxp with
    | Var v ->
      put_value v (AcRoot v)
    | Field (i, v) ->
      let acc = SMap.find v env.values in
      put_value key (AcField (acc, i))
    | Addition (i, v) ->
      let svalue = SMap.find v env.values in
      put_value key (AcField (svalue, i))
    | Function (v, sxp) ->
      let envf = put_value v (AcRoot v) in
      let c_tree = sym_exec sxp envf in
      put_function key (v, c_tree)
    | _ -> assert false
  in
  match sexpr with
  | Let (blist, next_sexpr) ->
    let env' = blist |>
               List.map (fun (var, sxp) -> eval_let_binding env sxp var) |>
               List.fold_left union env
    in
    sym_exec next_sexpr env'
  | If (bexpr, strue, sfalse) ->
    let piop, sxp =
      match bexpr with
      | Comparison (bop, sxp, i) -> (match_bop (bop, i)), sxp
      | Isout (i, v) -> Isout i, Var v
      | Var v -> Nq 0, Var v
      | _ -> assert false
    in
    let var = find_var env sxp in
    let pi = {var; op = piop} in
    Node ([
      (pi, sym_exec strue env);
      (negate_pi pi, sym_exec sfalse env)
    ], None)
  | Switch (sxp, swlist, defcase) ->
    let cases =
      let var = find_var env sxp in
      List.map (fun (test, sxp) -> {var; op = match_switch test}, sxp) swlist
    in
    let not_any_case = List.map negate_pi (List.map fst cases) in
    let children = List.map (fun (pi, sxp) -> (pi, sym_exec sxp env)) cases in
    let fallback = match defcase with
      | Some tree -> Some (not_any_case, sym_exec tree env)
      | None -> None
    in
    Node (children, fallback)
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
  | String s ->
     Manual_parser.print "Leaf String";
     Leaf s
  | Int n ->
     Leaf (string_of_int n)
  | TBlackbox t ->
    print_env env;
    Leaf t
  | Match_failure -> Failure
  | _ -> assert false

let empty_environment () =
  { values=SMap.empty; functions=SMap.empty; exits=IMap.empty; }

let eval target_ast =
  let _ = sym_exec target_ast (empty_environment ()) in
  ()

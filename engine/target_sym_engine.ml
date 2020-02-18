open Ast

let target_example = {|(let 
                        (r/1204 =
                            (function param/1206
                                (if (!= param/1206 1) (if (!= param/1206 2) "0" "2") "1"))))|}

module SMap = Map.Make(String)
module IMap = Map.Make(struct type t = int let compare = compare end)

(* efficient unions-of-intervals using the Diet library *)
module IntSet = Diet.Int

type constraint_tree =
  | Failure
  | Leaf of target_blackbox
  | Node of accessor * (domain * constraint_tree) list * (domain * constraint_tree) option
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
  | AcTag of accessor * int
  | AcAdd of accessor * int
and
  domain = { tag: IntSet.t; int: IntSet.t }

module Domain = struct
  module Set = struct
    let set interv = IntSet.add interv IntSet.empty
    let point n = set (IntSet.Interval.make n n)
    let interval low high = set (IntSet.Interval.make low high)

    let lt n = interval min_int (n - 1)
    let le n = interval min_int n
    let ge n = interval n max_int
    let gt n = interval (n+1) max_int

    let empty = IntSet.empty
    let full = interval min_int max_int
    let negate set = IntSet.diff full set
    let is_empty set = IntSet.is_empty set
    let is_full set = IntSet.is_empty (negate set)

    let equal = IntSet.equal

    let union = IntSet.union
    let inter = IntSet.inter

    let shift n set =
      let open IntSet in
      let shift_interval n intev =
        let open Interval in
        make (x intev + n) (y intev + n) in
      let on_interval intev acc =
        add (shift_interval n intev) acc in
      IntSet.fold on_interval set IntSet.empty
      (* TODO: this definition is incorrect in the case of overflows;
         for example
           x+1 <= 2
         gets translated to the set
           x+2 ∈ [min_int; 2]
         The version shifted by -1 should not be
           x ∈ [min_int-2; 0]
           (which is the nonsensical x ∈ [max_int; 1])
         but rather
           x ∈ [min_int; 0] ∪ [max_int-1; max_int]
       *)

    let to_string set =
      let on_interval interv acc =
        let low, high = IntSet.Interval.x interv, IntSet.Interval.y interv in
        let str =
          if low = high then string_of_int low
          else Printf.sprintf "[%s; %s]"
                 (if low = min_int then "-∞" else string_of_int low)
                 (if high = max_int then "+∞" else string_of_int high)
        in str :: acc
      in
      if is_empty set then "∅"
      else if is_full set then "_"
      else
        IntSet.fold on_interval set []
        |> List.rev
        |> String.concat " "
  end

  let int set = {int = set; tag = Set.empty}
  let tag set = {int = Set.empty; tag = set}

  let full = {int = Set.full; tag = Set.full}

  let is_empty dom = Set.is_empty dom.int && Set.is_empty dom.tag

  let equal dom dom' = Set.equal dom.int dom'.int && Set.equal dom.tag dom'.tag

  let negate dom = {
    int = Set.negate dom.int;
    tag = Set.negate dom.tag;
  }
  let union dom1 dom2 = {
    int = Set.union dom1.int dom2.int;
    tag = Set.union dom1.tag dom2.tag;
  }
  let inter dom1 dom2 = {
    int = Set.inter dom1.int dom2.int;
    tag = Set.inter dom1.tag dom2.tag;
  }

  let isin n = int (Set.interval 0 n)
  let isout n = negate (isin n)
  let isnot n = negate (int (Set.point n))

  let to_string {int; tag} =
    let show_int set = "Int " ^ Set.to_string set in
    let show_tag set = "Tag " ^ Set.to_string set in
    if Set.is_empty int && Set.is_empty tag then "false"
    else if Set.is_full int && Set.is_full tag then "true"
    else if Set.is_empty int then show_tag tag
    else if Set.is_empty tag then show_int int
    else Printf.sprintf "%s ∨ %s" (show_int int) (show_tag tag)

  let bprint buf domain =
    Printf.bprintf buf "%s" (to_string domain)
end

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
  let break ntabs buf =
    bprintf buf "\n%t" (indent ntabs)
  in
  let rec bprint_tree ntabs buf tree =
    match tree with
    | Failure ->
       bprintf buf "%tFailure" (indent ntabs)
    | Leaf target_blackbox ->
       bprintf buf "%tLeaf=%S\n" (indent ntabs) target_blackbox
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
  | Node (var, children, fallback) ->
     let subst (dom, tree) =
       (dom, subst_tree bindings tree)
     in
     Node (subst_svalue bindings var,
           List.map subst children,
           Option.map subst fallback)

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
  sym_exec target_ast (empty_environment ()) (* TODO: DISCUSS should return env? *)
(* What about all the functions in env? *)

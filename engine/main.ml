open Ast

let target_example = {|(let 
                        (r/1204 =
                            (function param/1206
                                (if (!= param/1206 1) (if (!= param/1206 2) "0" "2") "1"))))|}

type token = string
type input = token list
type 'a parser = input -> 'a * input

let tokenize lsp =
  lsp |> Str.global_replace (Str.regexp "(" ) " ( "
  |> Str.global_replace (Str.regexp ")" ) " ) "
  |> Str.global_replace (Str.regexp "\n" ) " "
  |> String.split_on_char ' '
  |> List.filter (fun c -> c <> "" && not(String.contains c ' '))

let print op = if false then Printf.printf "%s\n%!" op else ()

let rec parse_lambda : target_program parser = fun lsp ->
  let is_int_addition tk =
    Str.string_match (Str.regexp "-[0-9]+\\+") tk 0
  in
  let advance_two_sexpr lsp = (* helper function to read two sexpr at a time *)
    let s1, rem = parse_lambda lsp in
    let s2, rem' = parse_lambda rem in
    s1, s2, rem'
  in
  let consume_last_paren lsp = (* helper function to read a token expected to be ")" *)
    match lsp with
    | ")"::tl -> tl
    | [] -> print "ASSERT FAILURE: Nothing to consume"; assert false
    | x ->
       print ("ASSERT FAILURE IN "
              ^(String.concat " " (BatList.init 10 (List.nth x))));
       assert false
  in
  let rec advance_catch_exit_point lsp varlistr =
    (* When a catch expression is found,
     * the exit point and the variable list is parsed by this function
     * param lsp is of the form ["var1"; "var2"; ... ; "varn"; ")"]
     * varlist is used as an accumulator *)
    match lsp with
    | ")"::tl -> List.rev varlistr, tl
    | x::tl -> advance_catch_exit_point tl (x::varlistr)
    | _ -> assert false
  in
  let rec advance_switch_cases lsp cases_rev =
    (* Parses the cases of a switch expression
     * param lsp is a list containing tokens, some of which are "case" expression
     * the recursion terminates on "default" or on terminal paren
     * cases is used as an accumulator *)
    match lsp with
    | ")"::_ -> List.rev cases_rev, None, lsp
    | "case"::"int"::i::tl -> print "case int";
      let (i':int) = int_of_string (BatString.replace ~str:i ~sub:":" ~by:"" |> snd) in
      let sexpr, rem = parse_lambda tl in
      let (sw: switch_case) = Int i', sexpr in
      advance_switch_cases rem (sw::cases_rev)
    | "case"::"tag"::i::tl -> print "case tag" ;
      let (i':int) = int_of_string (BatString.replace ~str:i ~sub:":" ~by:"" |> snd) in
      let sexpr, rem = parse_lambda tl in
      let (sw: switch_case) = Tag i', sexpr in
      advance_switch_cases rem (sw::cases_rev)
    | "default:"::tl -> print "case default";
      let sexpr, rem = parse_lambda tl in
      List.rev cases_rev, Some sexpr, rem
    | _ -> assert false
  in
  let rec advance_exit_args rev_args lsp =
    match lsp with
      | ")" :: _ -> List.rev rev_args, lsp
      | rest ->
         let arg, rest = parse_lambda rest in
         advance_exit_args (arg :: rev_args) rest
  in
  let parse_list parse_elem rest =
    match rest with
      | "(" :: rest ->
         let rec loop acc = function
           | ")"::rest -> List.rev acc, rest
           | rest ->
              let elem, rest = parse_elem rest in
              loop (elem :: acc) rest
         in loop [] rest
      | _ -> assert false in
  let parse_let_bindings rest =
    let parse_binding = function
      | id :: ("=" | "=a") :: rest ->
         let def, rest = parse_lambda rest in
         (id, def), rest
      | _ -> assert false in
    parse_list parse_binding rest in
  let parse_special_form : sexpr parser = function
    | "setglobal" :: _ :: rest ->
       (* accept and ignore the "setglobal" call
          present at the top of examples *)
      parse_lambda rest
    | "makeblock"::_::_::rest -> TBlackbox "makeblock", rest
    | "let"::rest -> print "(let";
      let bindings, rest = parse_let_bindings rest in
      let body, rest = parse_lambda rest in
      Let (bindings, body), rest
    | "field"::i::v::tl -> print ("(field "^i^" "^v^")");
      begin
        match int_of_string_opt i with
        | Some i' -> Field (i', v), tl (*  TODO Could check if v is variable *)
        | _ -> assert false
      end
    | "function"::v::tl -> print ("(function "^v);
      let sexpr, rem = parse_lambda tl in
      Function (v, sexpr), rem
    | "if"::tl -> print "(if";
       let bexpr, tl = parse_lambda tl in
       let bexpr : bexpr = match bexpr with
         | Comparison (op, v, n) -> Comparison (op, v, n)
         | Field (i, v) -> Field (i, v)
         | Isout (i, v) -> Isout (i, v)
         | Var v -> Var v
         | _ ->
            assert false
       in
       let s1, s2, tl = advance_two_sexpr tl in
       If (bexpr, s1, s2), tl
    | "exit"::i::tl -> print ("exit "^i^" ");
      let i = int_of_string i in
      let args, tl = advance_exit_args [] tl in
      Exit (i, args), tl
    | ("switch"|"switch*")::tl -> print "(switch*)";
      let v, rem = parse_lambda tl in
      let cases, defcase, rem' = advance_switch_cases rem [] in
      Switch (v, cases, defcase), rem'
    | "catch"::tl -> print "(catch";
      let shead, rem = parse_lambda tl in
      let exitpoint, (varlist, rem') =
        match rem with
        | "with"::"("::i::tl -> int_of_string i, advance_catch_exit_point tl []
        | _ -> assert false
      in
      let stail, rem'' = parse_lambda rem'
      in
      Catch (shead, exitpoint, varlist, stail), rem''
    | "isout"::i::var::tl -> print ("isout"^i^" "^var);
      begin match int_of_string_opt i with
        | Some i -> Isout (i, var), tl
        | None -> assert false
      end
    | ((">"|"<"|">="|"<="|"=="|"!=") as bop)::tl -> print ("("^bop);  (* Comparison of bop * sexpr * int *)
      let s1, s2, rem = advance_two_sexpr tl in
      let op : target_program = match s2, bop with
        | Int i, ">" -> Comparison (Gt, s1, i)
        | Int i, "<" -> Comparison (Lt, s1, i)
        | Int i, ">=" -> Comparison (Ge, s1, i)
        | Int i, "<=" -> Comparison (Lt, s1, i)
        | Int i, "==" -> Comparison (Eq, s1, i)
        | Int i, "!=" -> Comparison (Nq, s1, i)
        | _ -> assert false
      in op, rem
    | addint::var::tl when is_int_addition addint ->
      begin match int_of_string_opt (BatString.rchop addint) with
        | Some i -> Addition (i, var), tl
        | None -> assert false
      end
    | other :: _ -> print ("Failure on "^other); assert false
    | [] -> assert false
  in
  match lsp with
  | (("true"|"false") as b)::")"::tl -> Bool (bool_of_string b), tl
  | "("::rest ->
    let expr, rest = parse_special_form rest in
    expr, consume_last_paren rest
  | x::tl -> 
     begin match int_of_string_opt x with
       | Some i -> print ("Int: "^x); Int i, tl
       | None ->
         assert (x <> ")") ;
         if x <> "" && x.[0] = '"' then
           (print ("String: "^x); assert (x.[String.length x - 1] = '"'); String x, tl)
         else (print ("Var: "^x); Var x, tl)
     end
  | _ -> assert false

let parse_file filename =
  let target_example = BatFile.with_file_in filename BatIO.read_all in
  let tk = tokenize target_example in
  let sexpr, tl = parse_lambda tk in
  if tl <> [] then Printf.eprintf "unparsed: %S\n%!" (String.concat " " tl); sexpr

let example_ast =
  parse_file Sys.argv.(1)

module SMap = Map.Make(String)
module IMap = Map.Make(struct type t = int let compare = compare end)

type constraint_tree =
  | Leaf of pi list * target_blackbox
  | Node of constraint_tree list
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

let rec sym_exec sexpr constraints env : constraint_tree =
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
  let extract_leaves tree =
    let rec extract accum = function
      | Leaf _ as l -> l::accum
      | Node n -> (List.map (extract []) n |> List.flatten)@accum
    in
    extract [] tree
  in
  let rec subst_svalue bindings = function
    | AcRoot v -> begin
        match List.assoc_opt v bindings with
        | Some svalue -> svalue
        | None -> AcRoot v
      end
    | AcField (acc', i) -> AcField (subst_svalue bindings acc', i)
    | AcTag (acc', i) -> AcTag (subst_svalue bindings acc', i)
    | AcAdd (svalue', i) -> AcAdd (subst_svalue bindings svalue', i)
  in
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
  let break ntabs buf =
    bprintf buf "\n%s" (BatList.init ntabs (fun _ -> "\t") |> String.concat "") in
  let rec bprint_tree ntabs buf tree =
    let sep = break ntabs in
    match tree with
    | Leaf (pilist, target_blackbox) ->
       bprintf buf
         "Leaf=%s\
              %t%a"
         target_blackbox
         (break ntabs) (bprint_list ~sep bprint_pi) pilist
    | Node (cst_list) ->
       bprintf buf
         "Node=\
          %t%a"
         (break ntabs)
         (bprint_list ~sep (bprint_tree (ntabs+1))) cst_list
  and
  bprint_env ntabs buf env =
    let bprint_function buf binding =
      let (k, fn) = binding in
        let (v, f_tree) = fn
        in
        bprintf buf "%s: Function=%s,ConstraintTree: %a"
          k v
        (bprint_tree (ntabs + 1)) f_tree
      in
      let bprint_exits buf binding =
      let (k, catch) = binding in
        let (e, vars, c_tree) = catch
        in
        bprintf buf "%d: Catch=%d %s,ConstraintTree: %a"
          k e
          (String.concat " " vars)
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
  let print_env env =
    let buf = Buffer.create 42 in
    bprint_env 0 buf env;
    BatIO.write_line BatIO.stdout (Buffer.contents buf)
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
      let c_tree = sym_exec sxp constraints envf in
      put_function key (v, c_tree)
    | _ -> assert false
  in
  match sexpr with
  | Let (blist, next_sexpr) ->
    let env' = blist |>
               List.map (fun (var, sxp) -> eval_let_binding env sxp var) |>
               List.fold_left union env
    in
    sym_exec next_sexpr constraints env'
  | If (bexpr, strue, sfalse) ->
    let piop, sxp =
      match bexpr with
      | Comparison (bop, sxp, i) -> (match_bop (bop, i)), sxp
      | Isout (i, v) -> Isout i, Var v
      | _ -> assert false (* TODO *)
    in
    let avar = match sxp with
      | Var v -> SMap.find v env.values
      | _ -> assert false
    in
    let child1 = sym_exec strue ({var=avar; op=piop}::constraints) env
    in
    let child2 = sym_exec sfalse ({var=avar; op=negate piop}::constraints) env
    in
    Node [child1; child2;]
  | Switch (sxp, swlist, defcase) ->
    let var = match sxp with
      | Var v -> SMap.find v env.values
      | _ -> assert false
    in
    let constraintsxtargets = List.map (fun (test, sxp) -> (match_switch test, sxp)) swlist
    in
    let children = constraintsxtargets |>
                   List.map (fun (c, target) -> sym_exec target ({var=var; op=c}::constraints) env)
    in
    begin
      match defcase with
      | Some defcase ->
        let defcase_constraints = constraintsxtargets |>
                                  List.map (fun (c, _) -> {var=var; op=negate c})
        in
        Node (children @ [sym_exec defcase (defcase_constraints @ constraints) env])
      | None -> Node children
    end
  | Catch (sxp, extpt, varlist, exit_sxp) ->
    let c_tree = sym_exec exit_sxp constraints env in
    let env' = put_exit extpt (extpt, varlist, c_tree) in
    sym_exec sxp constraints env'
  | Exit (ext, evalues) ->
    let innervars = evalues |> List.map (fun (v : sexpr) -> match v with
        | Var inner -> inner
        | _ -> assert false)
    in
    let innersvalues = innervars |> List.map (fun v -> SMap.find v env.values)
    in
    let branch =
      let (ext', vars, c_tree) = IMap.find ext env.exits in
      assert (ext' = ext);
      assert (List.length vars = List.length innersvalues);
      let bindings = List.combine vars innersvalues
      in
      let leaves = extract_leaves c_tree
      in
      let new_leaves = leaves |>
                       List.map (function
                           | Leaf (pis, t) ->
                             let pis' = List.map
                                 (fun pi -> { pi with var = subst_svalue bindings pi.var }) pis
                             in
                             Leaf (pis', t)
                           | _ -> assert false)
      in
      if (List.length new_leaves) = 1 then
        List.hd new_leaves
      else
        Node new_leaves
    in
    branch
  | String s -> print "Leaf String"; Leaf (constraints, s)
  | TBlackbox t ->
    print_env env;
    Leaf (constraints, t)
  | _ -> assert false

let empty_environment () =
  { values=SMap.empty; functions=SMap.empty; exits=IMap.empty; }

let () =
  let _ = sym_exec example_ast [] (empty_environment ())  in
  ()

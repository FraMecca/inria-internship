[@@@ warning "-30"]

type source_program  = {
  scrutinee: variable;
  clauses: clause list;
}
and
  clause = pattern * source_expr
and
  pattern =
  | Wildcard
  | Constructor of constructor * pattern list
  | Or of pattern * pattern
  | As of pattern * variable
and
  constructor =
  | Variant of string
  | Int of int
  | Bool of bool
  | Tuple
  | Nil
  | Cons
and
  variable = string
and 
  source_expr = SBlackbox of source_blackbox
and
  source_blackbox = string

type target_program = sexpr
and
  sexpr =
  | Var of variable
  | Int of int
  | Bool of bool
  | String of string
  | Addition of int * variable 
  | Function of variable * sexpr
  | Let of binding list * sexpr
  | Catch of sexpr * exitpoint * variable list * sexpr
  | Exit of exitpoint * sexpr list (* could be "exit 1 var1 var2" *)
  | If of bexpr * sexpr * sexpr
  | Switch of sexpr * switch_case list * sexpr option
  | Field of int * variable
  | Comparison of bop * sexpr * int
  | Isout of int * variable
  | TBlackbox of target_blackbox
and
  binding = variable * sexpr
and
  exitpoint = int
and
  bexpr =
  | Comparison of bop * sexpr * int
  | Field of int * variable
  | Isout of int * variable
  | Var of variable
and
  switch_case = switch_test * sexpr
and
  switch_test =
  | Tag of int
  | Int of int
and
  target_blackbox = string
and
  bop =
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Nq

type source_constraint =
  | Wildcard
  | Constructor of constructor * source_constraint list
  | As of source_constraint * variable

type target_constraints = a_constraint list
and
  a_constraint = qualifier * aop * index
and
  aop =
  | Immediate of bop * int
  | Tag of int
  | Isrange of int list
and
  index =
  | Root
  | Field of int * index
and
  qualifier = bool


let target_example = {|(let 
                        (r/1204 =
                            (function param/1206
                                (if (!= param/1206 1) (if (!= param/1206 2) "0" "2") "1"))))|}

let tokenize lsp =
  lsp |> Str.global_replace (Str.regexp "(" ) " ( "
  |> Str.global_replace (Str.regexp ")" ) " ) "
  |> Str.global_replace (Str.regexp "\n" ) " "
  |> String.split_on_char ' '
  |> List.filter (fun c -> c <> "" && not(String.contains c ' '))

let print op = if false then Printf.printf "%s\n%!" op else ()

let rec parse_lambda lsp =
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
  let parse_special_form = function
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
      let op = match s2, bop with
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
  ignore target_example;
  let target_example = BatFile.with_file_in filename BatIO.read_all in
  let tk = tokenize target_example in
  let sexpr, tl = parse_lambda tk in
  if tl <> [] then Printf.eprintf "unparsed: %S\n%!" (String.concat " " tl); sexpr

let example_ast =
  parse_file Sys.argv.(1)

module SMap = Map.Make(String)

type constraint_tree =
  | Leaf of pi list * target_blackbox * environment SMap.t
  | Node of constraint_tree list
  | Jump of pi list  * exitpoint * environment SMap.t
and
  pi = { var: variable; op: piop }
and
  environment =
  | Accessor of accessor
  | Addition of variable * int
  | Function of variable * constraint_tree 
  | Catch of exitpoint * constraint_tree
and
  accessor =
  | AcRoot of variable
  | AcField of variable * int
  | AcTag of variable * int
and
  piop =
  | Tag of int
  | NotTag of int
  | Int of int
  | NotInt of int
  | Ge of int
  | Gt of int
  | Le of int
  | Lt of int
  | Eq of int
  | Nq of int
  | Isout of int
  | Isin of int

let rec sym_exec sexpr constraints env : constraint_tree =
  let match_bop: (bop * int -> piop) = function
    | Ge, i -> Ge i
    | Gt, i -> Gt i
    | Le, i -> Le i
    | Lt, i -> Lt i
    | Eq, i -> Eq i
    | Nq, i -> Eq i
  in
  let match_switch: (switch_test -> piop) = function
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
  let pi_to_str pi =
    let op = match pi.op with
      | Tag i -> "Tag "^(string_of_int i)
      | NotTag i -> "NotTag "^(string_of_int i)
      | Int i-> "Int "^(string_of_int i)
      | NotInt i-> "NotInt "^(string_of_int i)
      | Ge i -> "Ge "^(string_of_int i)
      | Gt i -> "Gt "^(string_of_int i)
      | Le i -> "Le "^(string_of_int i)
      | Lt i -> "Lt "^(string_of_int i)
      | Eq i -> "Eq "^(string_of_int i)
      | Nq i -> "Nq "^(string_of_int i)
      | Isout i -> "Isout "^(string_of_int i)
      | Isin i -> "Isin "^(string_of_int i)
    in
    "{ var="^pi.var^"; op="^op^"; }"
  in
  let rec tree_to_str ntabs tree =
    let sep = "\n"^(BatList.init ntabs (fun _ -> "\t") |>  String.concat "" ) in
    match tree with
    | Leaf (pilist, target_blackbox, l_env) ->
      let l_str = "Leaf="^target_blackbox^sep^(List.map (fun pi -> pi_to_str pi) pilist |> String.concat sep)
      in
      let e_str = (BatList.init (ntabs-1) (fun _ -> "\t") |>  String.concat "" )^
                  "+> with env:"^sep^(env_to_str l_env ntabs)
      in
      l_str^"\n"^e_str
    | Node (cst_list) ->
      "Node="^sep^(List.map (fun t -> tree_to_str (ntabs+1) t) cst_list |> String.concat sep)
    | Jump (pilist, ext, _) ->
      "Jump="^(string_of_int ext)^(List.map (fun pi -> pi_to_str pi) pilist |> String.concat sep)
  and
  env_to_str env ntabs =
    let sep = "\n"^(BatList.init ntabs (fun _ -> "\t") |>  String.concat "" ) in
    SMap.bindings env |>
    List.map (fun (key, entry) ->
        let value = match entry with
          | Accessor a -> begin
              match a with
              | AcRoot (v) -> "AcRoot="^v
              | AcField (v, i) -> "AcField="^v^","^(string_of_int i)
              | AcTag (v, i) -> "AcTag="^v^","^(string_of_int i)
            end
          | Addition (v, i) -> "Addition="^v^","^(string_of_int i)
          | Function (v, f_tree) -> "Function="^v^",ConstraintTree: "^(tree_to_str (ntabs+1) f_tree)
          | Catch (e, c_tree) ->"Catch="^string_of_int e^",ConstraintTree: "^(tree_to_str (ntabs+1) c_tree)
        in
        key^": "^value
      ) |> String.concat sep
  in
  let print_env env =
    let str_env = env_to_str env in
    BatIO.write_line BatIO.stdout (str_env 0)
  in
  let expand_env variable (entry: environment) =
    let _ = match SMap.find_opt variable env with
      | Some _ -> assert false
      | None -> ()
    in
    SMap.add variable entry env
  in
  (* perform union on two maps, keys should never clash *)
  let union env1 env2 = SMap.union (fun _ a b -> assert (a = b); Some a) env1 env2
  in
  let match_let_accessor = function
    | Var v -> Accessor (AcRoot v)
    | Field (i, v) -> Accessor (AcField (v, i))
    | Function (v, sxp) ->
      let constraint_tree = sym_exec sxp constraints env in
      Function (v, constraint_tree)
    | String _ -> assert false
    | TBlackbox _ -> assert false
    | Int _ -> assert false
    | Bool _ -> assert false
    | Addition (i, v)  ->  Addition (v, i)
    | Let _ -> assert false
    | Catch _ -> assert false
    | Exit _ -> assert false
    | If _ -> assert false
    | Switch _ -> assert false
    | Comparison _ -> assert false
    | Isout _ -> assert false
  in
  match sexpr with
  | Let (blist, next_sexpr) ->
    let env' = blist |>
               List.map (fun (var, sxp) -> expand_env var (match_let_accessor sxp)) |>
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
    let svar = match sxp with
      | Var v -> v
      | _ -> assert false
    in
    let child1 = sym_exec strue ({var=svar; op=piop}::constraints) env
    in
    let child2 = sym_exec sfalse ({var=svar; op=negate piop}::constraints) env
    in
    Node [child1; child2;]
  | Switch (sexpr, swlist, defcase) ->
    let var =
      match sexpr with
      | Var v -> v;
      | _ -> assert false
    in
    let constraintsxtargets = List.map (fun c -> (match_switch (fst c), snd c)) swlist
    in
    let defcase_constraints = List.map (fun (c,_) -> {var=var; op=negate c}) constraintsxtargets
    in
    let children = constraintsxtargets |>
                   List.map (fun (c, target) -> sym_exec target ({var=var; op=c}::constraints) env)
    in
    begin
      match defcase with
      | Some defcase -> Node (children@[sym_exec defcase (defcase_constraints@constraints) env])
      | None -> Node children
    end
  | Catch (sxp, extpt, _, sxp') ->
    let c_tree = sym_exec sxp' constraints env in
    let env' = expand_env (string_of_int extpt) (Catch (extpt, c_tree)) in
    sym_exec sxp constraints env'
  | Exit (i, _) -> Jump (constraints, i, env)
  | String s -> print "Leaf String"; Leaf (constraints, s, env)
  | Isout _ -> assert false
  | Addition _ -> assert false
  | Field _ -> assert false
  | Int _ -> assert false
  | Comparison _ -> assert false
  | Var _ -> assert false
  | Bool _ -> assert false
  | Function _ -> assert false
  | TBlackbox t ->
    print_env env;
    Leaf (constraints, t, env)

let () =
  let _ = sym_exec example_ast [] SMap.empty in
  ()

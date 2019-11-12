[@@@ warning "-30"]
open BatList

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
  | Function of variable * sexpr
  | Let of variable * sexpr
  | Catch of sexpr * exitpoint * variable list * sexpr
  | Exit of exitpoint * sexpr list (* could be "exit 1 var1 var2" *)
  | If of bexpr * sexpr * sexpr
  | Switch of sexpr * switch_case list * sexpr option
  | Field of int * variable
  | Comparison of bop * sexpr * int
  | TBlackbox of target_blackbox
and
  exitpoint = int
and
  bexpr =
  | Comparison of bop * sexpr * int
  | Field of int * variable
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

type eng = { stm: string; pi: bool }

let rec parse lsp =
  let print op = Printf.printf "%s\n%!" op
  in
  let advance_two_sexpr lsp = (* helper function to read two sexpr at a time *)
    let s1, rem = parse_lambda lsp in
    let s2, rem' = parse_lambda rem in
    s1, s2, rem'
  in
  let consume_last_paren lsp = (* helper function to read a token expected to be ")" *)
    match lsp with
    | ")"::tl -> tl
    | x -> print ("ASSERT FAILURE IN "^(List.hd x)); assert false
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
    | ")"::tl -> List.rev cases_rev, None, tl
    | _ -> assert false
  in
  let parse_lambda_special_form = function
    | "let"::"("::v::("="|"=a")::tl -> print ("(let("^v);
      let s1, rem = parse_lambda tl in
      Let (v, s1), consume_last_paren rem (* TODO: manage multiple sexprs, eg: (let s1 s2 s3 ... sn) *)
    | "field"::i::v::tl -> print ("(field "^i^" "^v^")");
      begin
        match int_of_string_opt i with
        | Some i' -> Field (i', v), tl (*  TODO Could check if v is variable *)
        | _ -> assert false
      end
    | "function"::v::tl -> print ("(function "^v);
      let sexpr, rem = parse_lambda tl in
      Function (v, sexpr), rem
    | "exit"::i::tl -> print ("exit "^i^" ");
      let i' =
        match int_of_string_opt i with
        | Some i' -> i'
        | _ -> assert false
      in
      let split_index_opt = BatList.index_of ")" tl in
      let split_index = match split_index_opt with | Some i -> i | None -> assert false in
      let r, l = BatList.split_at split_index tl in
      let sexpr_list = r |> BatList.filter (fun c -> c <> ")") |>  BatList.map (fun v -> Var v) in
      Exit (i', sexpr_list ), l

    | ("switch"|"switch*")::tl -> print "(switch(*))";
      let v, rem =
        match tl with
        | "("::_ -> parse_lambda tl
        | x::_ -> Var x, tl
        | [] -> assert false
      in
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
    | _ -> assert false
  in
  match lsp with
  | (("true"|"false") as b)::")"::tl -> Bool (bool_of_string b), tl
  | "("::((">"|"<"|">="|"<="|"=="|"!=") as bop)::tl -> print ("("^bop);  (* Comparison of bop * sexpr * int *)
    let s1, s2, rem = advance_two_sexpr tl in
    begin
      match s2, bop with
      | Int i, ">" -> Comparison (Gt, s1, i), rem
      | Int i, "<" -> Comparison (Lt, s1, i), rem
      | Int i, ">=" -> Comparison (Ge, s1, i), rem
      | Int i, "<=" -> Comparison (Lt, s1, i), rem
      | Int i, "==" -> Comparison (Eq, s1, i), rem
      | Int i, "!=" -> Comparison (Nq, s1, i), rem
      | _ -> assert false
    end
  | "("::"if"::tl -> print "(if";
    let bexpr, rem = parse_lambda tl in
    let s1, s2, rem' = advance_two_sexpr rem in
    begin
      match bexpr with
      | Comparison (a, b, c) -> If (Comparison (a, b, c), s1, s2), rem'
      | Field (a, b) -> If (Field (a, b), s1, s2), rem' (* TODO: why can't use "as"??? *)
      | _ -> assert false (* Bexpr can be only a sexpr of type Comparison|Field *)
    end
  | "("::rest ->
    let expr, rest = parse_lambda_special_form rest in
    expr, consume_last_paren rest
  | x::")"::tl ->
    begin
      match int_of_string_opt x with
      | Some i -> Int i, tl
      | None -> TBlackbox x, tl (* TODO: should be Var x? How to distinguish? *)
    end
  | x::tl when x <> ")" -> print ("Var "^x); Var x, tl
  | _ -> assert false


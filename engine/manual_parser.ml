open Ast

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

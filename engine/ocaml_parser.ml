module Ocaml_ast = Migrate_parsetree.Ast_408

type ocaml_program = Ocaml_ast.Parsetree.structure

let parse_implementation lexbuf =
  match Ocaml_common.Parse.implementation lexbuf with
  | exception exn -> Error exn
  | ast ->
    let convert =
      let open Migrate_parsetree.Versions in
      (migrate ocaml_current ocaml_408).copy_structure in
    Ok (convert ast)

let handle_error exn =
  Ocaml_common.Location.report_exception Format.err_formatter exn;
  exit 2

let ocaml_of_file file =
  let input = open_in file in
  Fun.protect ~finally:(fun () -> close_in input) @@ fun () ->
  let lexbuf = Lexing.from_channel input in
  Ocaml_ast.Location.init lexbuf file;
  Ocaml_ast.Location.input_name := file;
  Ocaml_ast.Location.input_lexbuf := Some lexbuf;
  parse_implementation lexbuf

let pp_ocaml_program ppf (prog : ocaml_program) =
  let convert =
    let open Migrate_parsetree.Versions in
    (migrate ocaml_408 ocaml_current).copy_structure in
  Ocaml_common.Pprintast.structure ppf (convert prog)

let pp_ocaml_expr ppf (exp : Ocaml_ast.Parsetree.expression) =
  let convert =
    let open Migrate_parsetree.Versions in
    (migrate ocaml_408 ocaml_current).copy_expression in
  Ocaml_common.Pprintast.expression ppf (convert exp)

module Location = Ocaml_ast.Location
open Ocaml_ast.Parsetree

let error_at loc fmt =
  Location.raise_errorf ~loc ("OCaml->AST conversion error: " ^^ fmt)

open Ast

let rec ast_of_ocaml ~file (prog : ocaml_program) : source_program =
  let definition =
    match prog with
    | [] | _ :: _ :: _ ->
       error_at (Location.in_file file)
         "a single declaration was expected"
    | [def] ->
       def in
  let definition_body =
    match definition.pstr_desc with
    | Pstr_value (_, [binding]) ->
       binding.pvb_expr
    | _ ->
       error_at definition.pstr_loc
         "a value declaration was expected"
  in
  let cases =
    match definition_body.pexp_desc with
    | Pexp_function cases ->
       cases
    | Pexp_fun (_arg_label, _default_val, arg, body) ->
       let error loc = error_at loc "(fun x -> match x with ...) expected" in
       let param = bound_var_of_ocaml arg in
       let cases = match body.pexp_desc with
         | Pexp_match ({ pexp_desc = Pexp_ident id; pexp_loc; _ }, cases) ->
            if not (String.equal (ident_of_ocaml id) param)
            then error pexp_loc;
            cases
         | _ -> error body.pexp_loc
       in cases
    | _ ->
       error_at definition_body.pexp_loc
         "expected (function ...) or (fun x -> match x with ...)"
  in
  {
    clauses = List.map clause_of_ocaml cases;
  }

and clause_of_ocaml { pc_lhs = pattern; pc_guard = guard; pc_rhs = expr } : clause =
  begin match guard with
  | None -> ()
  | Some guard ->
     Location.alert ~kind:"Warning" guard.pexp_loc "Ignored pattern guard";
  end;
  let pattern = pattern_of_ocaml pattern in
  let expr = blackbox_of_ocaml expr in
  (pattern, expr)

and pattern_of_ocaml p : Ast.pattern =
  let error fmt = error_at p.ppat_loc fmt in
  match p.ppat_desc with
  | Ppat_any ->
     Wildcard
  | Ppat_var { Location.txt = var; _ } ->
     As (Wildcard, var)
  | Ppat_alias (p, { Location.txt = var; _ }) ->
     As (pattern_of_ocaml p, var)
  | Ppat_or (p1, p2) ->
     Or (pattern_of_ocaml p1, pattern_of_ocaml p2)
  | Ppat_constant cst ->
     let cstr: constructor =
       match constant_of_ocaml (Location.mkloc cst p.ppat_loc) with
       | `Int n -> Int n
       | `String s -> String s
     in
     Constructor (cstr, [])
  | Ppat_construct (cstr, params) ->
     let cstr = constructor_of_ocaml cstr in
     let args = match params with
       | None ->
          []
       | Some { ppat_desc = Ppat_tuple args; _ } ->
          List.map pattern_of_ocaml args
       | Some p ->
          [pattern_of_ocaml p]
     in
     Constructor (cstr, args)
  | Ppat_tuple args ->
     let args = List.map pattern_of_ocaml args in
     Constructor (Tuple, args)
  | _ -> error "unsupported pattern"

and ident_of_ocaml lid =
  let error fmt = error_at lid.Location.loc fmt in
  let open Ocaml_ast.Longident in
  match lid.Location.txt with
  | Ldot _ | Lapply _ -> error "unsupported identifier"
  | Lident id -> id

and constructor_of_ocaml cstr =
  match ident_of_ocaml cstr with
  | "true" -> Bool true
  | "false" -> Bool false
  | "[]" -> Nil
  | "::" -> Cons
  | variant -> Variant variant

and constant_of_ocaml (cst : constant Location.loc) =
  let error fmt = error_at cst.Location.loc fmt in
  match cst.Location.txt with
  | Pconst_integer (n, None) ->
     `Int (try int_of_string n
           with _exn -> error "invalid integer literal")
  | Pconst_string (str, _) ->
     `String str
  | Pconst_integer (_, Some _) (* 32n, 32L etc. *)
  | Pconst_char _
  | Pconst_float _ -> error "unsupported literal"

and bound_var_of_ocaml arg =
  let error fmt = error_at arg.ppat_loc fmt in
  match arg.ppat_desc with
    | Ppat_var name -> name.Location.txt
    | _ -> error "a single bound variable was expected"

and blackbox_of_ocaml exp =
  match exp.pexp_desc with
    | Pexp_constant (Pconst_string (str, _)) -> SBlackbox str
    | _ -> SBlackbox "<expr>"

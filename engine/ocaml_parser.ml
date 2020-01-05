module Ocaml_ast = Migrate_parsetree.Ast_408

let parse_implementation lexbuf : Ocaml_ast.Parsetree.structure =
  let ast =
    try Ocaml_common.Parse.implementation lexbuf
    with exn ->
      Ocaml_common.Location.report_exception Format.err_formatter exn;
      exit 2 in
  let convert =
    let open Migrate_parsetree.Versions in
    (migrate ocaml_current ocaml_408).copy_structure in
  convert ast

let try_parse file =
  let input = open_in file in
  Fun.protect ~finally:(fun () -> close_in input) @@ fun () ->
  let lexbuf = Lexing.from_channel input in
  Ocaml_ast.Location.init lexbuf file;
  ignore (parse_implementation lexbuf)

let source_exec file =
  let ocaml_ast =
    match Ocaml_parser.ocaml_of_file file with
    | Ok ocaml_ast -> ocaml_ast
    | Error err ->
      Ocaml_parser.handle_error err
  in
  let ast =
    try Ocaml_parser.ast_of_ocaml ~file ocaml_ast with
    | exn -> Ocaml_parser.handle_error exn in
  Source_sym_engine.eval ast

let target_exec file =
  let ast =
    match Menhir_parser.parse_file file with
    | Ok ast -> ast
    | Error (lexbuf, _exn) ->
      Printf.eprintf "%s: Syntax error.\n%!" (Menhir_parser.location_message lexbuf);
      exit 1
  in
  Target_sym_engine.eval ast

let () =
  let target_tree_with_accessors = target_exec Sys.argv.(1) in
  Target_sym_engine.print_tree target_tree_with_accessors;
  let target_tree = Merge_accessors.merge target_tree_with_accessors in
  assert (Equivalence.compare target_tree target_tree)

let () =
  if Array.length Sys.argv >= 3 then begin
    let file = Sys.argv.(2) in
    let result = source_exec file in
    Source_sym_engine.print_result result
  end

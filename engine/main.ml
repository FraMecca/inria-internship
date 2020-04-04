let () =
  if Array.length Sys.argv < 3 then failwith "Not enough arguments";

  let target = 
    match Menhir_parser.parse_file Sys.argv.(1) with
    | Error (lexbuf, _exn) ->
       Printf.eprintf "%s: Syntax error.\n%!" (Menhir_parser.location_message lexbuf);
       failwith "Menhir parser error"
    | Ok ast ->
       let tree = Target_sym_engine.eval ast in
       Target_sym_engine.print_tree tree ;
       Merge_accessors.merge tree
  in
  let source = 
    let file = Sys.argv.(2) in
    match Ocaml_parser.ocaml_of_file file with
    | Error err -> Ocaml_parser.handle_error err
    | Ok ocaml_ast ->
       match Ocaml_parser.ast_of_ocaml ~file ocaml_ast with
       | exception exn -> Ocaml_parser.handle_error exn
       | ast ->
          Format.printf "Source input:@.%a@." Ocaml_parser.pp_ocaml_program ocaml_ast;
          Source_sym_engine.eval ast
  in
  let repr_env = Source_env.ConstructorMap.empty in
  let repr_env = Source_env.ConstructorMap.add "()" (Source_env.Int 0) repr_env in
  let result = Equivalence_source_target.compare repr_env source target in
  BatIO.write_string BatIO.stdout ("Result: "^(string_of_bool result))

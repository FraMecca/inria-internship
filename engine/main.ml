let () =
  match Menhir_parser.parse_file Sys.argv.(1) with
    | Error (lexbuf, _exn) ->
       Printf.eprintf "%s: Syntax error.\n%!" (Menhir_parser.location_message lexbuf);
       exit 1
    | Ok ast ->
      let tree = Target_sym_engine.eval ast in
      Target_sym_engine.print_tree tree;
      let _ = Merge_accessors.merge tree in ()

let () =
  if Array.length Sys.argv >= 3 then
    let file = Sys.argv.(2) in
    match Ocaml_parser.ocaml_of_file file with
    | Error err -> Ocaml_parser.handle_error err
    | Ok ocaml_ast ->
      match Ocaml_parser.ast_of_ocaml ~file ocaml_ast with
      | exception exn -> Ocaml_parser.handle_error exn
      | ast ->
        Format.printf "Source input:@.%a@." Ocaml_parser.pp_ocaml_program ocaml_ast;
        Source_sym_engine.eval ast

let () =
  match Menhir_parser.parse_file Sys.argv.(1) with
    | Error (lexbuf, _exn) ->
       Printf.eprintf "%s: Syntax error.\n%!" (Menhir_parser.location_message lexbuf);
       exit 1
    | Ok ast ->
      let result = Target_sym_engine.eval ast |> Merge_accessors.merge in
      assert (Equivalence.compare result result)

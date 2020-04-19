type source_result = {
  type_decls: Ast.type_decl list;
  source_tree: Source_sym_engine.constraint_tree;
}

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
  let type_decls = ast.type_decls in
  let source_tree = Source_sym_engine.eval ast in
  {
    type_decls;
    source_tree;
  }

type target_result = {
  target_tree_with_accessors: Target_sym_engine.constraint_tree;
  target_tree: Merge_accessors.constraint_tree;
}

let target_exec file =
  let ast =
    match Menhir_parser.parse_file file with
    | Ok ast -> ast
    | Error (lexbuf, _exn) ->
      Printf.eprintf "%s: Syntax error.\n%!" (Menhir_parser.location_message lexbuf);
      exit 1
  in
  let target_tree_with_accessors = Target_sym_engine.eval ast in
  let target_tree = Merge_accessors.merge target_tree_with_accessors in
  {
    target_tree_with_accessors;
    target_tree;
  }

type compare_result = {
  source: source_result;
  target: target_result;
  comparison: bool;
}

let compare source_file target_file =
  let source = source_exec source_file in
  let target = target_exec target_file in
  let repr_env =
    Source_env.build_type_repr_env source.type_decls in
  let comparison =
    Equivalence_source_target.compare repr_env
      source.source_tree
      target.target_tree
  in
  {
    source;
    target;
    comparison;
  }

let () =
  let target_file = Sys.argv.(1) in
  if Array.length Sys.argv <= 2 then begin
    (* no source file provided *)
    let result = target_exec target_file in
    print_endline "Target program constraint trees";
    Target_sym_engine.print_tree result.target_tree_with_accessors;
    print_newline ();
  end else begin
    let source_file = Sys.argv.(2) in
    let result = compare source_file target_file in
    let () =
      print_endline "Target program constraint tree";
      Target_sym_engine.print_tree result.target.target_tree_with_accessors;
      print_newline ();

      print_endline "Source program constraint tree";
      Source_sym_engine.print_result result.source.source_tree;
      print_newline ();
    in
    if result.comparison then begin
      print_endline "The two programs are equivalent.";
      exit 0
    end else begin
      print_endline "The two programs are NOT equivalent.";
      exit 1
    end
  end

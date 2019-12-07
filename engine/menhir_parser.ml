type location = {
  path: string;
  line: int;
  column: int;
}

let location_message lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d"
    pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_file path =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let lexbuf = Lexing.from_channel ic in
  Lexer.init path lexbuf;
  match Menhir_grammar.program Lexer.token lexbuf with
    | program -> Ok program
    | exception exn -> Error (lexbuf, exn)

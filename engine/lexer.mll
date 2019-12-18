{
open Menhir_grammar

let init fname lexbuf =
  let open Lexing in
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let curr_pos lexbuf = lexbuf.Lexing.lex_curr_p
let set_start_pos lexbuf pos = lexbuf.Lexing.lex_start_p <- pos

let new_line lexbuf =
  let open Lexing in
  let p = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { p with
    pos_lnum = p.pos_lnum + 1;
    pos_bol = p.pos_cnum;
  }
}

let int = "-"? ['0'-'9'] ['0'-'9' '_']*
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let newline = ('\r'* '\n')
let blank = [' ' '\t']+

rule token = parse
  | newline
    { new_line lexbuf;
      token lexbuf }
  | blank { token lexbuf }

  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { EQUAL }
  | "=a" { EQUALA }
  | "!=" { BANGEQUAL }
  | "<" { LESS }
  | ">" { GREATER }
  | "<=" { LESSEQUAL }
  | ">=" { GREATEREQUAL }
  | ":" { COLON }

  | "case" { CASE }
  | "catch" { CATCH }
  | "exit" { EXIT }
  | "field" { FIELD }
  | "function" { FUNCTION }
  | "if" { IF }
  | "int" { INTSYMBOL }
  | "isout" { ISOUT }
  | "let" { LET }
  | "makeblock" { MAKEBLOCK }
  | "setglobal" { SETGLOBAL }
  | "switch" { SWITCH }
  | "switch*" { SWITCHSTAR }
  | "tag" { TAGSYMBOL }
  | "with" { WITH }

  | (int as n) { INT (int_of_string n) }
  | "false" { BOOL false }
  | "true" { BOOL true }
  | "\""
    { string (curr_pos lexbuf) (Buffer.create 42) lexbuf }

  | (int as n) "+" { OFFSET (int_of_string n) }

  | (lowercase identchar* '/' int) as ident { LIDENT ident }
  | ('*' lowercase identchar* '*' '/' int) as ident { LIDENT ident }
  | (uppercase identchar* '!') as ident { UIDENT ident }

  | eof { EOF }

and string start_p buf = parse
  | "\""
    { set_start_pos lexbuf start_p;
      STRING (Buffer.contents buf) }
  | '\\' (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c)
    { Buffer.add_char buf (match c with
                             | 'n' -> '\n'
                             | 't' -> '\t'
                             | 'b' -> '\b'
                             | 'r' -> '\r'
                             | _ -> c);
      string start_p buf lexbuf }
  | (_ as c)
    { Buffer.add_char buf c;
      string start_p buf lexbuf }

%start <Ast.target_program> program
%type <Ast.sexpr> sexp atom body
%token <bool> BOOL
%token <string> STRING
%token <int> INT
%token <int> INTA
%token <int> OFFSET
%token <Ast.variable> LIDENT
%token <Ast.variable> UIDENT

%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token EQUALA
%token LESS
%token LESSEQUAL
%token GREATER
%token GREATEREQUAL
%token EQUAL
%token BANGEQUAL
%token COLON COMMA STAR

%token CASE
%token CATCH
%token EXIT
%token FALSE
%token FIELD
%token FUNCTION
%token IF
%token ISOUT
%token LET
%token MAKEBLOCK
%token SETGLOBAL
%token GLOBAL
%token RAISE
%token SWITCH
%token SWITCHSTAR
%token INTSYMBOL
%token TAGSYMBOL
%token TRUE
%token WITH
%token EOF

%token APPLY OBSERVE GUARD

%{
open Ast

let is_match_failure str =
  let pat = "Match_failure/" in
  let patlen = String.length pat in
  String.length str >= patlen
  && String.sub str 0 patlen = pat
%}

%%

let program :=
| e = sexp; EOF; <>

let sexp :=
| id=LIDENT; { (Var id: sexpr) }
| atom
| parens(body)

let parens(rule) == LPAREN; ~=rule; RPAREN; <>

let atom :=
| b=BOOL; { Bool b }
| n=INT; { Int n }
| s=STRING; { String s }

let body :=
| (* accept and ignore the "setglobal" call
     present at the top of examples *)
 SETGLOBAL; _=UIDENT;
  exp=parens(
    LET;
    (_,module_item)=parens(let_binding);
    block(sexp);
    {module_item});
  <>
| RAISE; LPAREN;
    MAKEBLOCK; _=INT;
      LPAREN; GLOBAL; exn=UIDENT; RPAREN;
      _=block(sexp);
  RPAREN;
  { if is_match_failure exn then (Match_failure : sexpr)
    else failwith "'raise' parse error" }
| LET; ~=let_bindings; ~=sexp; <Let>
| FUNCTION; x=variable; option(COLON; typename); body=sexp; <Function>
| IF; cond=bexp; then_=sexp; else_=sexp; <If>
| IF; guard_args=parens(apply(GUARD,value)); then_=sexp; else_=sexp; <IfGuard>
| EXIT; exit=INT; args=list(v=variable; { (Var v : sexpr) }); <Exit>
| CATCH; scrutinee=sexp;
  WITH; LPAREN; exit=INT; vars=list(variable); RPAREN; exit_body=sexp;
  <Catch>
| offset=OFFSET; v=variable; <Addition>
| SWITCHSTAR; scrutinee=sexp; cases=switch_cases; { Switch(scrutinee, cases, None) }
| bexp = bexp_body; { sexpr_of_bexpr bexp }
| observe_args=apply(OBSERVE,value); <TBlackbox>

let switch_cases == list(switch_case)
let switch_case :=
| CASE; ~=switch_test; COLON; ~=sexp; <>
let switch_test :=
| TAGSYMBOL; n=INT; <Tag>
| INTSYMBOL; n=INT; <Int>

let bexp :=
| id = LIDENT; { (Var id: bexpr) }
| parens(bexp_body)

let bexp_body :=
| FIELD; index=INT; arg=variable; <Field>
| ISOUT; i=INT; v=variable; <Isout>
| ~=bop; ~=sexp; n=INT; <Comparison>

let bop :=
| LESS; { Lt }
| GREATER; { Gt }
| LESSEQUAL; { Le }
| GREATEREQUAL; { Ge }
| EQUAL; { Eq }
| BANGEQUAL; { Nq }

let apply(Fun,Arg) :=
  | Fun; v=Arg; { [v] }
  | APPLY; first=parens(Fun; first=Arg; <>); rest=list(Arg); { first :: rest }

let block(Arg) :=
| LBRACKET; tag=INT; COLON; args=list(Arg); RBRACKET; <>
| parens(MAKEBLOCK; tag=INT; _=block_shape; args=list(Arg); <>)

let block_shape == ioption(parens(separated_nonempty_list(COMMA, word_shape)))
let word_shape := INTSYMBOL | STAR

let value :=
| n=INT; { VConstant n }
| v=variable; { VVariable v }
| (tag,args) = block(value); { VConstructor {tag; args} }

let typename :=
| INTSYMBOL; {}
| variable; {}

let let_bindings == parens(list(let_binding))
let let_binding :=
| id=LIDENT; midrule(EQUAL | EQUALA); def=sexp; <>

let variable == LIDENT

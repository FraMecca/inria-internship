%start <Ast.target_program> program
%type <Ast.sexpr> sexp atom body
%token <bool> BOOL
%token <string> STRING
%token <int> INT
%token <int> OFFSET
%token <Ast.variable> LIDENT
%token <Ast.variable> UIDENT

%token LPAREN
%token RPAREN
%token EQUALA
%token LESS
%token LESSEQUAL
%token GREATER
%token GREATEREQUAL
%token EQUAL
%token BANGEQUAL
%token COLON

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
%token SWITCH
%token SWITCHSTAR
%token INTSYMBOL
%token TAGSYMBOL
%token TRUE
%token WITH
%token EOF

%{
open Ast
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
| SETGLOBAL; _=UIDENT; rest=sexp;
  (* accept and ignore the "setglobal" call
     present at the top of examples *)
  <>
| MAKEBLOCK; tag=INT; args=list(sexp);
  { ignore (tag, args); TBlackbox "makeblock" }
| LET; ~=let_bindings; ~=sexp; <Let>
| FUNCTION; x=variable; body=sexp; <Function>
| IF; cond=bexp; then_=sexp; else_=sexp; <If>
| EXIT; exit=INT; args=list(v=variable; { (Var v : sexpr) }); <Exit>
| CATCH; scrutinee=sexp;
  WITH; LPAREN; exit=INT; vars=list(variable); RPAREN; exit_body=sexp;
  <Catch>
| offset=OFFSET; v=variable; <Addition>
| SWITCHSTAR; scrutinee=sexp; cases=switch_cases; { Switch(scrutinee, cases, None) }
| bexp = bexp_body; { sexpr_of_bexpr bexp }

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

let let_bindings == parens(list(let_binding))
let let_binding :=
| id=LIDENT; midrule(EQUAL | EQUALA); def=sexp; <>

let variable == LIDENT

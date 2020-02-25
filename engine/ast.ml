[@@@ warning "-30"]

type source_program  = {
  type_decls: type_decl list;
  clauses: clause list;
}
and
  clause = pattern * source_rhs
and
  source_rhs =
  | Unreachable (* OCaml refutation clauses: | Foo -> . *)
  | Expr of source_expr
and
  pattern =
  | Wildcard
  | Constructor of constructor * pattern list
  | Or of pattern * pattern
  | As of pattern * variable
and
  constructor =
  | Variant of variant_name
  | Int of int
  | Bool of bool
  | String of string
  | Tuple of int
  | Nil
  | Cons
and
  variant_name = string
and
  variable = string
and
  source_expr = SBlackbox of source_blackbox
and
  source_blackbox = string
and type_decl = {
  name: type_name;
  constructors: constructor_decl list;
}
and
  type_name = string
and constructor_decl = {
  constructor_name: variant_name;
  args: type_expr list;
}
and type_expr = unit (* we only care about the arity for now *)

type target_program = sexpr
and
  sexpr =
  | Var of variable
  | Int of int
  | Bool of bool
  | String of string
  | Addition of int * variable
  | Function of variable * sexpr
  | Let of binding list * sexpr
  | Catch of sexpr * exitpoint * variable list * sexpr
  | Exit of exitpoint * sexpr list (* could be "exit 1 var1 var2" *)
  | If of bexpr * sexpr * sexpr
  | Switch of sexpr * switch_case list * sexpr option
  | Field of int * variable
  | Comparison of bop * sexpr * int
  | Isout of int * variable
  | Match_failure
  | TBlackbox of target_blackbox
and
  binding = variable * sexpr
and
  exitpoint = int
and
  bexpr =
  | Comparison of bop * sexpr * int
  | Field of int * variable
  | Isout of int * variable
  | Var of variable
and
  switch_case = switch_test * sexpr
and
  switch_test =
  | Tag of int
  | Int of int
and
  target_blackbox = string
and
  bop =
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Nq

type source_constraint =
  | Wildcard
  | Constructor of constructor * source_constraint list
  | As of source_constraint * variable

type accessor =
  | AcRoot
  | AcField of accessor * int

let sexpr_of_bexpr : bexpr -> sexpr = function
| Comparison (op, exp, n) -> Comparison (op, exp, n)
| Field (i, v) -> Field (i, v)
| Isout (i, v) -> Isout (i, v)
| Var v -> Var v

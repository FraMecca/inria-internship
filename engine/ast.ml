[@@@ warning "-30"]

type source_program  = {
  type_decls: type_decl list;
  clauses: clause list;
}
and
  clause = pattern row
and 'a row = {
    lhs: 'a;
    guard: guard option;
    rhs: source_rhs;
}
and guard =
  | Guard of source_value list
and
  source_rhs =
  | Unreachable (* OCaml refutation clauses: | Foo -> . *)
  | Observe of source_value list
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
  | Unit
and
  variant_name = string
and
  variable = string
and
  source_value =
  | VConstructor of constructor * source_value list
  | VVar of variable
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
  | IfGuard of target_value list * sexpr * sexpr
  | If of bexpr * sexpr * sexpr
  | Switch of sexpr * switch_case list * sexpr option
  | Field of int * variable
  | Comparison of bop * sexpr * int
  | Isout of int * variable
  | Match_failure
  | TBlackbox of target_value list
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
  target_value = 
  | VConstructor of {tag:int; args:target_value list}
  | VVariable of variable
  | VConstant of int
and
  bop =
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Nq

type accessor =
  | AcRoot
  | AcField of accessor * int

type source_constraint =
  | Wildcard
  | Constructor of constructor * source_constraint list
  | As of source_constraint * variable

let sexpr_of_bexpr : bexpr -> sexpr = function
| Comparison (op, exp, n) -> Comparison (op, exp, n)
| Field (i, v) -> Field (i, v)
| Isout (i, v) -> Isout (i, v)
| Var v -> Var v

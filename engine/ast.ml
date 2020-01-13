[@@@ warning "-30"]

type source_program  = {
  clauses: clause list;
}
and
  clause = pattern * source_expr
and
  pattern =
  | Wildcard
  | Constructor of constructor * pattern list
  | Or of pattern * pattern
  | As of pattern * variable
and
  constructor =
  | Variant of string
  | Int of int
  | Bool of bool
  | String of string
  | Tuple of int
  | Nil
  | Cons
and
  variable = string
and 
  source_expr = SBlackbox of source_blackbox
and
  source_blackbox = string

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

type target_constraints = a_constraint list
and
  a_constraint = qualifier * aop * index
and
  aop =
  | Immediate of bop * int
  | Tag of int
  | Isrange of int list
and
  index =
  | Root
  | Field of int * index
and
  qualifier = bool

let sexpr_of_bexpr : bexpr -> sexpr = function
| Comparison (op, exp, n) -> Comparison (op, exp, n)
| Field (i, v) -> Field (i, v)
| Isout (i, v) -> Isout (i, v)
| Var v -> Var v

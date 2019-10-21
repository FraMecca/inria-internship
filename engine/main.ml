[@@@ warning "-30"]
open BatIO

type source_program  = {
  scrutinee: variable;
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
  | Tuple
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
  | Function of variable * sexpr
  | Let of variable * sexpr * sexpr
  | Catch of sexpr * exitpoint * variable list * sexpr
  | Exit of exitpoint * sexpr list (* could be "exit 1 var1 var2" *)
  | If of bexpr * sexpr * sexpr
  | Switch of variable * switch_case list * sexpr option
  | Field of int * variable
  | Comparison of bop * sexpr * int
  | TBlackbox of target_blackbox
and
  exitpoint = int
and
  bexpr =
  | Comparison of bop * sexpr * int
  | Field of int * variable
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

# Objective

The tool verifies if the compiler provided a lambda representation of the pattern matching section of the code that is coherent with the source code.

Coherent in this context means that for the same input both the targets return the same value or expression.

# Inputs of the tool

The tool receives as its inputs an ocaml pattern matching section of code and AST of the Lambda code produced by the compiler.

The pattern matching section has form:

```
match x with
|p1 -> e1
|p2 -> e2
...
|pn -> en
```

and can include any expression that is legal for the ocaml compiler, such as "when" conditions and assignments.
Patterns could or could not be exhaustive.

# Output of the tool

The tool returns an empty output in case the equivalence between the target language and the source language is satisfied, otherwise it specifies which patterns in the source language are not satisfied in the target language.

# Methodology

### Source 

Both inputs (meaning the lambda code and the pattern matching code) will be evaluated symbolically in order to model with a set of constraints the patterns expressed in the source language and the execution paths expressed in the target language.

In the context of the source language the expressions:

```
|p1 -> e1
|p2|p3 -> e2
...
|pn -> en 
```

will be symbolically represented as:

```
[| ρ1 |] -> e1
[| ¬ρ1 ∧ ρ2 |] -> e2
[| ¬ρ1 ∧ ρ3 |] -> e2
...
[| (¬ρ1∧¬ρ2∧¬ρ3∧...∧¬ρn-1)∧ρn |]
```

where [| (¬ρ1∧¬ρ2∧¬ρ3∧...∧¬ρn-1)∧ρn |] is called a ρ-constraint.
The result of the symbolic execution of the first input will result in a set of tuples of the form
(ρ-constraint, o-expression), where a o-expression is treated as black box ocaml code.

ρ-constraints are the result of the consecutive application of the four rules (variable rule, constructor rule, orpat rule, mixture rule) on the clause matrix P->L.

In particular, each rule is mapped to a ρ-constraint in the following way (the index of the column is omitted):
* constructor rule -> given the constructor c, ρi matches the constructor c and doesn't match every constructor c' != c
* variable rule -> given variable yi for row i, ρi is equal to the variable yi and not equal to every variable yj for j != i
* orpat rule -> given that the orpat rules adds one or more row to a single row matrix, evaluation continues after duplicating the ρ-constraints gathered until that point
* the mixture rule produces a new ρ-constraint that is the negation of the current ρ-constraint

Also, it should be noted that symbolic execution engine branches whenever the mixture rule is applied.

TODO discuss: maybe duplication in the orpat rule is not needed, maybe depends on implementation.

### Target 

In the context of the target language the tool evaluates symbolically the code in lambda form.
Branching is performed in the context of:
* if clauses
* switch clauses

Branching allows the construction of a tree where every node represents a constraint on the symbolic variable.
In particular, when branching on an "if" clause two children are mapped to the current node;
for a switch case there will be as many children as "case" statements, plus one if the "default" case is present.
Leafs are constructed when the evaluation reduces to a result value.
For if cases the two branches will have two constraints, one the negation of the other.
For switch* clauses there will be one branch for each "case" clause with a new constraint generated from it.
In case of switch clauses, the same will apply plus there will be a branch representing the "default case" as a negation of the union of the constraints on the precedente branches (¬(c4 ∨ c5 ∨ ... ∨ cn)).

% Discussion:
% "exit" labels allow jumps during the execution. Jumps are always performed in inner frames and always advance execution (loops are not legal). This means that a topological sort is always possible and a DAG could be constructed.
% Sub trees could be constructed bottom up, by evaluating the target code starting from the outermost "with" label: when a jump happens, the subtree can be copied and attached as the child of the node.
% I propose for the moment to ignore this fact and in case of a jump, to just repeat the evaluation.
% Subtrees constructed by the repeated evaluation in case of jumps will differ for the intersection of the constraints obtained before the jump.

% [Gabriel] an easy approach is to do caching/memoization: if you see
% (exit i), you first compute the path starting from (i) (or reuse it
% if previously computed) and you prepend your own current path to it
% to get the result. This follows the "naive" control-flow but with
% the "bottom-up" efficiency.

The tree will have this form:

```
                          *
                          |
                          |
                       if clause
                         /\
                       c1  ¬c1
                       /    \
            switch clause   ...
            /   |       \
           /    |        \
          /     |         \
      c1∧c3  c1∧c4        c1∧c5    ...   c1∧cn-1   c1∧¬(c4∨...∨cn-1)
        |        |             |          |              |
        |        |             |          |              |
       e1       e2            e2         en-1         en (default case)
```

The result of the symbolic execution of the second input will result in a set of tuples of the form
(ι-constraint, l-expression), where l-expression is arbitrary code in lambda form, contained in one of the leafs of the constructed tree, while ι-constraints is a list of "arithmetic constraints" contained in the parent of the leaf.
A arithmetic constraints is of the form `(op variable number)` where op is one of {!=, ==, <=, >=} and are constructed when branching on a proposition in the lambda code.

## Equivalence

Defining SourceSet the set of tuples (ρ-constraint, o-expression) and TargetSet the set of tuples (ι-constraint, l-expression), the last step for checking the equivalence of the source and the target inputs consists in checking the equivalence of:
* (1) o-expressions with l-expressions
* (2) ι-contraints with ρ-constraint

Regarding (1), the ocaml compiler already supports the translation (e -> l as noted in the paper).

% [Gabriel] I think that we could assume given a relation between
% o-expressions and l-expressions as a side-result of the compiler run
% that we are verifying. (This is conceptually nicer than running the
% compiler again.)

% [Gabriel] for a toy model of the problem we could restrict the
% language of o-expressions and l-expressions to make the
% correspondence trivial. For example each right-hand-side could be of
% the form
%
%     observe case-number x y z ... (the captured variables)

Regarding (2), it can be noted that structural constraints contains information on the constructor because of the encoding of the type constructors by the ocaml compiler.
Example:
```
type t = K1 of int | K2 | K3 of int| K4
(* will be mapped to:
 * K1 -> tag 0,
 * K2 -> int 0,
 * K3 -> tag 1,
 * K4 -> int 1
 *)
```

Patterns are represented as a tree of accessors (`field n variable`).
Example:

```
Some (A 9, B "st");;
```

```
[0: [0: [0: 9] [1: "st"]]] 
                                      *
                                      |
                                      0     (field 0 ~)
                                      |
                                      0     (field 0 (field 0 ~))
                                     / \ 
  (field 1 (field 0 (field 0 ~)))   0   1   (field 1 (field 0 (field 0 ~)))
                                   /     \
                 (field 0 (...))  9     "st"   (field 0 (...))
```


This means that when executing the lambda code we can map each proposition (represented as a constraint of the ι-constraint) with one node part of the above tree that represents the ρ-constraint that we are testing against.

A node satisfies a ι-constraint when 
* called c the constraint on the node
* the application of the value of the node on c and ¬c reduces respectively to true and false.

# High level representation

                                            +------------------+
                                            |  Input:          |
                                            |        source    |
                                            |        target    |
                                            +------------------+
                                                     |
               "match x with |p1|p2|..."             |         lambda code
                                      +╌╌╌╌╌╌╌╌╌╌╌╌╌╌*╌╌╌╌╌╌╌╌╌╌╌╌╌╌+
                                      |                             |
                                      |                             |
                            +------------------+           +------------------+
                            |   clause matrix  |           |     Symbolic     |
                            |      P -> L      |           |     excution     |
                            +------------------+           +------------------+
                                      |                             |
                                      |                             |
                            +------------------+                    |
                            |     Symbolic     |                    |
                            |     excution     |                    |
                            +------------------+                    |
                                      |                             |
     ((ρ1,o1), (ρ2,o2), ..., (ρn,on)) +╌╌╌╌╌╌╌╌╌╌╌╌╌╌*╌╌╌╌╌╌╌╌╌╌╌╌╌╌+ ((ι1,l1), (ι2,l2), ..., (ιn,ln))
                                                     |
                                                     |
                             +--------------------------------------------------+
                             |  (o_expr -> l_expr) -> o_expr -> l_expr -> bool  |
                             |  (ι_ctr -> ρ_ctr)   -> ι_ctr  -> ρ_ctr  -> bool  |
                             |          transform and check equivalence         |
                             +--------------------------------------------------+
                                                     |
                                                     v 
                                                  result!

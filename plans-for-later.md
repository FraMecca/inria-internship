Also checking variables:

  leaves (on both side) should have a leaf environment
    (variable -> accessor)*

  in the symbolic execution function, keep the "current environment"
    (already done in the target symexec, todo in the source symexec)

  in the equivalence checker

     Leaf (source_env, source_bb), Leaf (target_env, target_bb) ->
       source_env ⊆ target_env  and  source_bb = target_bb
       ∀(x ↦ a) ∈ source_env, target_env(x) = a


Guards

  Simplifying assumption (proposed): assume that all the when-guards
    are compiled to (apply ... ...) in the lambda-code; we only support
    guards of this shape.

  new kind of constraint-tree node

     Guard((binding_env, bb), C_true, C_false)

  Francesco: while checking equivalence, we maintain queue of guards (along with the input space)
  - if you find a guard on the left, you push to the queue
  - if you find a guard on the right, you pop (if it's the same guard as the top of the queue, otherwise you fail)
  - if you find a leaf, the queue must be empty


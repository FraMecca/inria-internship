Also checking variables:

  leaves (on both side) should have a leaf environment
    (variable -> accessor)*

  in the symbolic execution function, keep the "current environment"
    (already done in the target symexec, todo in the source symexec)

  in the equivalence checker

     Leaf (source_env, source_bb), Leaf (target_env, target_bb) ->
       source_env ⊆ target_env  and  source_bb = target_bb
       ∀(x ↦ a) ∈ source_env, target_env(x) = a

Remove unreachable

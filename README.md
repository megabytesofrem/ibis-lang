# Ibis

Ibis is an experimental programming language designed to explore the application of category theory to memory safety

## Notes

Required tactics to add to the elaborator for theorem proving:

Core: 
- intro/revert          : bind and unbind variables in goal context
- exact/assumption      : use a term that exactly matches the goal or an assumption
- rfl/simp              : use reflexivity or simplification to solve equalities
- apply/elim            : apply a lemma or eliminate a hypothesis
- cases/induction       : perform case analysis or induction on a term
- rw/subst              : rewrite using an equality
- sorry                 : admit a goal (placeholder for future proof)

Presheaves & Sheaves:
- path_across           : dependent path equality proof
- covers_j              : grothendieck site cover analysis
- res/restrict V        : restrict a section along an arrow
- lan/extend V          : extend a section along a left Kan extension
- glue V W              : glue two sections along a cover if they agree on the overlap

Tactic syntax
```
def prove_equality (x : A) (y : A) : x = y := by
  intro h
  rw h
  rfl
```

Normal syntax
```
def add (x : Nat) (y : Nat) : Nat := x + y
def sub (x : Nat) (y : Nat) : Nat := x - y
```
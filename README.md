# Ibis

Ibis is a dependently typed language with a syntax inspired by Lean and Agda built on top 
of the Calculus of Inductive Constructions (CIC). It extends the CIC with additional constructs for working with presheaves and sheaves and embeds a full compile-time topos
engine as a 'compile-time borrow checker' to reason about memory safety.

It is a highly experimental language and targets embedded devices which otherwise would
be limited to C99.

## Notes

Required tactics to add to the elaborator for theorem proving:

Core: 
- intro                 : introduce a new variable into the context
- exact e               : provide an exact term to solve the goal
- apply e               : apply a term to the goal, generating new subgoals
- rfl                   : prove equality by reflexivity
- simp e                : simplify the goal using the given term
- cases e               : perform case analysis on the given term
- induction e           : perform induction on the given term
- let x : A = e in t    : introduce a let binding into the context
- have h : A := e in t  : introduce a hypothesis into the context
- show e                : show the type of the given term
- sorry                 : admit the current goal

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
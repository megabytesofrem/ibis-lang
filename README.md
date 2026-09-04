# Ibis

Ibis is a dependently typed language with a syntax inspired by Lean and Agda built on top 
of the Calculus of Inductive Constructions (CIC). It extends the CIC with additional constructs for working with presheaves and sheaves and embeds a full compile-time topos
engine as a 'compile-time borrow checker' to reason about memory safety.

It is a highly experimental language and targets embedded devices which otherwise would
be limited to C99.

## Current TODOs
- Fully implement Millers Higher Order Pattern Unification algorithm (`Ibis.Typecheck.Unify.Solver`)
  for solving unification problems in the elaborator.

- Wire up the unification solver to the elaborator and implement a tactic system inspired by Lean 4
  for proving theorems and constructing terms.


## AI Transparency
Large Language Models (LLMs) are used solely as a tool to assist with the following tasks:
- *Paper Translation*: Decompiling dense, cryptic papers into reference algorithms for implementation.
- *Documentation*: Assisting with formatting and writing documentation for the code-base.
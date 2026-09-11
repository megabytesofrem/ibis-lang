# Ibis

Ibis is an experiment to build a topos-theoretic programming language, to explore the application of
both category theory and topology to programming language design and memory safety.

## Architecture
The architecture of Ibis is foreign to any other programming language, because of the infinite nature of
the underlying mathematical model that Ibis is based on.

- **WorldServer**: A concurrent server that maintains the Grothendieck site topology and serves chunks of the topology to clients, via STM queues
- **WorldGen**: Procedural world generations that works in-tandem with **WorldServer** to generate and cache chunks of the topology on-demand, bypassing the issue of storing an infinite topology in memory.
- **Debugger**: A Minecraft 1.16.5 client-server protocol that communicates bi-directionally with the WorldServer to visualize the Grothendieck site topology in 3D, and to allow for interactive exploration of the topology.

## Design Tradeoffs
- Lean style syntax: A pipe dream. Gone by necessity and replaced with Haskell and Pascal
style `interface/implementation` syntax — because we have **no** EOF.
- Modules: Gone, they are impossible. Hope you like netlists! Because 4 levels of netlist is the *best* you get, because the compiler is single pass due to the laws of physics.

## Current TODOs
- Fully implement Millers Higher Order Pattern Unification algorithm (`Ibis.Typecheck.Unify.Solver`)
  for solving unification problems in the elaborator.

- Wire up the unification solver to the elaborator and implement a tactic system inspired by Lean 4
  for proving theorems and constructing terms.

- Wire up **WorldServer** to store/cache chunks in a anvil-like format

## AI Transparency
Large Language Models (LLMs) are used solely as a tool to assist with the following tasks:
- *Paper Translation*: Decompiling dense, cryptic papers into reference algorithms for implementation.
- *Documentation*: Assisting with formatting and writing documentation for the code-base.
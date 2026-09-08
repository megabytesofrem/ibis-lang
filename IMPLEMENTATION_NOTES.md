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

## Infinite Topos Engine
Currently the topos engine is a work in progress, and is at stage 0.
```
Stage 0: Preliminary Architecture (Active)
  └── Establish core category-theoretic types (`Grothendieck.hs`, `CoreTerm`)
  └── Define CLI interfaces, spatial viewports, and C99 lowering goals

Stage 1: Finite Topoi & Local Sheafification
  └── In-memory site topologies & presheaf section evaluation (`Sect`, `Res`, `Ext`)
  └── Exact boundary condition checking (`glue`) across bounded spatial covers
  └── Direct C99 basic block emission via `LowerCFG`

Stage 2: Infinite Topoi (Streaming & Chunking)
  └── `.gtop` region file serialization inspired by Minecraft `.mca` files
  └── Monadic `IO` streaming pipeline with an LRU sub-site sector cache
  └── Configurable `--render-distance` evaluation radius
```
Ideas:
- Grothendieck site caching inspired by Minecraft RCA region files for caching topoi/subtopoi to disk
- Lazy streaming of topoi/subtopoi from disk to memory for large topoi, just like Minecraft chunk streaming
- Render distance to control how many topoi/subtopoi are loaded into memory
```
└── artifacts
  └── world
    └── region
      └── r.0.0.itop
      └── r.0.1.itop
      └── r.1.0.itop
      └── r.1.1.itop
    └── level.dat
    └── cursor.dat
└── dist
  └── out.c
```
This will require the AST to now be co-inductive, and streamed from disk, with a cursor to the current position in the world.

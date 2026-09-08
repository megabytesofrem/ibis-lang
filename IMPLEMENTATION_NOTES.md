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

## Implementation of the World Gen
Problem: We represent memory as covers within a Grothendieck site, forming a Grothendieck topos. The topos is by definition infinite, and we cannot store the entire topos in memory at once. 

Solution: We use Minecraft logic, and treat the compiler as a game server. 

```
┌────────────────────────┐      ┌─────────────────────────┐      ┌──────────────────────────────┐
│  Surface AST           │ ───> │  Co-inductive CoTerm    │ ───> │  Section CoTerm c            │
│  (Finite 1D Tree)      │      │  (Infinite Observation) │      │  (Embedded Spatial Payload)  │
└────────────────────────┘      └─────────────────────────┘      └──────────────────────────────┘
```

### Phase 1: Static Surface AST
**Goal**: Parse source code into the surface AST.
**State in RAM**: A standard AST in memory
**Purpose**: This acts purely as an input seed to the next phase

### Phase 2: Co-inductive AST
**Goal**: Convert the surface AST into a co-inductive AST. We do this by doing the following:

1. Extract site rules (`SiteDecl` nodes) to define the topological rules and site covers of our
Grothendieck site.
2. Assign top level coordinates: Every top-level declaration is assigned a spatial coordinate in the world.
3. Lift terms into sections: Every top-level declaration is lifted into a section of the presheaf over the Grothendieck site. We say that the term is "spatially located" at the coordinate assigned to it, and the presheafs payload is the term itself.

```hs
data ASTSection c = ASTSection
  { sectionCoord :: SectorCoord
  , sectionTerm :: CoTerm
  }
```

## Phase 3: Infinite Streaming Topos
Now the `WorldGen` takes over. Terms are expanded and lowered on demand based on an internal
camera position so long as it is within a render distance of the camera.

1. Camera raymarches: As the observer moves through the world, the camera raymarches through the world and requests sections of the presheaf at the coordinates it is currently observing.
2. Local expansion: If a term contaisn an unexpanded site-cover (`Cover u v`, `Res u v`) or recursive types, `WorldGen` generates adjacent voxel chunks containing the expanded sub-terms
3. Code emission: The local slices of the topos (within the render distance) are lowered to C
basic blocks and written into a byte-buffer.
4. Garbage collection: Once the camera steps past the render distance, any slices of the topos 
that are no longer within the render distance are garbage collected and removed from memory.
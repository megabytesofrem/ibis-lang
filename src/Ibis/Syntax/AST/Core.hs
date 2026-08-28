-- | Core AST for the Ibis programming language.
module Ibis.Syntax.AST.Core
  ( -- * Debruijn indices
    DeBruijn

    -- * Core Terms
  , CoreTerm (..)
  , CoreDef (..)
  , CoreProgram (..)

    -- * NbE
  , Value (..)
  , Neutral (..)
  )
where

import Ibis.Syntax.AST.Surface (Literal (..), Pat (..))

-- | De Bruijn index for variables and type variables.
type DeBruijn = Int

type Closure = (Value -> Value)

data CoreTerm
  = Universe Int
  | Var DeBruijn
  | Lit Literal
  | Unit
  | -- Dependent Functions
    Pi (Maybe String) CoreTerm CoreTerm -- Π(A). B
  | Lam (Maybe String) CoreTerm -- λ. body
  | App CoreTerm CoreTerm -- f x
  -- Dependent Products
  | Sigma (Maybe String) CoreTerm CoreTerm -- Σ(A). B
  | Pair CoreTerm CoreTerm -- (a, b)
  | Fst CoreTerm -- fst p
  | Snd CoreTerm -- snd p
  -- Pattern Matching
  | Match CoreTerm [(Pat, CoreTerm)]
  deriving (Show, Eq)

data Value
  = VUniverse Int
  | VLit Literal
  | VUnit
  | -- Dependent Functions
    VPi (Maybe String) Value Closure
  | VLam (Maybe String) Closure
  | -- Dependent Products
    VSigma (Maybe String) Value Closure
  | VPair Value Value
  | -- Stuck
    VNeutral Value Neutral

data Neutral
  = NVar Int
  | NApp Neutral Value
  | NFst Neutral
  | NSnd Neutral
  | NMatch Neutral [(Pat, Value)]

data CoreDef = CoreDef
  { defName :: String -- String retained for diagnostic & symbol linking
  , defType :: CoreTerm -- Evaluated to Value during type checking
  , defBody :: CoreTerm -- Fully desugared core term
  }
  deriving (Eq, Show)

-- | Core Program: Top-level environment/module
newtype CoreProgram = CoreProgram [CoreDef]
  deriving (Eq, Show)
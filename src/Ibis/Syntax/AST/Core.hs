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

import Data.List (intercalate)
import Ibis.Syntax.AST.Surface (Literal (..), Pat (..))

-- | De Bruijn index for variables and type variables.
type DeBruijn = Int

-- | Closure type for functions in the NbE evaluator.
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
  deriving (Eq)

data Value
  = VUniverse Int -- Universe levels (types are terms, universes classify types)
  | VLit Literal
  | VUnit
  | -- Dependent Functions
    VPi (Maybe String) Value Closure
  | VLam (Maybe String) Value Closure
  | -- Dependent Products
    VSigma (Maybe String) Value Closure
  | VPair Value Value
  | -- Stuck
    VNeutral Value Neutral

-- Neutral terms represent computations that are "stuck" on a variable or a neutral term,
-- which cannot be further evaluated.
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

---------------------------------------------
-- SHOW INSTANCES
---------------------------------------------

instance Show CoreTerm where
  show (Universe n) = "Type " ++ show n
  show (Var n) = "v" ++ show n
  show (Lit l) = show l
  show Unit = "()"
  show (Pi mName dom cod) =
    let nameStr = maybe "_" id mName
     in "(Π(" ++ nameStr ++ ": " ++ show dom ++ "). " ++ show cod ++ ")"
  show (Lam mName body) =
    let nameStr = maybe "_" id mName
     in "(λ(" ++ nameStr ++ "). " ++ show body ++ ")"
  show (App f x) = "(" ++ show f ++ " " ++ show x ++ ")"
  show (Sigma mName dom cod) =
    let nameStr = maybe "_" id mName
     in "(Σ(" ++ nameStr ++ ": " ++ show dom ++ "). " ++ show cod ++ ")"
  show (Pair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (Fst p) = "(fst " ++ show p ++ ")"
  show (Snd p) = "(snd " ++ show p ++ ")"
  show (Match e branches) =
    let branchStrs = map (\(pat, body) -> show pat ++ " -> " ++ show body) branches
     in "(match " ++ show e ++ " with { " ++ intercalate "; " branchStrs ++ " })"

instance Show Value where
  show (VUniverse n) = "Type " ++ show n
  show (VLit l) = show l
  show VUnit = "()"
  show (VPi mName dom cod) =
    let nameStr = maybe "_" id mName
     in "(Π(" ++ nameStr ++ ": " ++ show dom ++ "). " ++ "<closure>)"
  show (VLam mName dom f) =
    let nameStr = maybe "_" id mName
     in "(λ(" ++ nameStr ++ ": " ++ show dom ++ "). " ++ "<closure>)"
  show (VSigma mName dom cod) =
    let nameStr = maybe "_" id mName
     in "(Σ(" ++ nameStr ++ ": " ++ show dom ++ "). " ++ "<closure>)"
  show (VPair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (VNeutral v neu) = "<neutral: " ++ show neu ++ ">"

instance Show Neutral where
  show (NVar n) = "v" ++ show n
  show (NApp neu v) = "(" ++ show neu ++ " " ++ show v ++ ")"
  show (NFst neu) = "(fst " ++ show neu ++ ")"
  show (NSnd neu) = "(snd " ++ show neu ++ ")"
  show (NMatch neu branches) =
    let branchStrs = map (\(pat, val) -> show pat ++ " -> " ++ show val) branches
     in "(match " ++ show neu ++ " with { " ++ intercalate "; " branchStrs ++ " })"

----------------------------------------------

-- | Core Program: Top-level environment/module
newtype CoreProgram = CoreProgram [CoreDef]
  deriving (Eq, Show)
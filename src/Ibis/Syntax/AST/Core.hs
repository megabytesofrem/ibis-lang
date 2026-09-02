{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
  Module      : Ibis.Syntax.AST.Core
  Description : Core syntax of the Ibis language
-}
module Ibis.Syntax.AST.Core
  ( -- * Debruijn indices
    Index (..)
  , Level (..)
  , Spine

    -- * Core Terms
  , CoreTerm (..)
  , CoreDecl (..)
  , CoreProgram (..)

    -- * NbE
  , Value (..)
  , Neutral (..)
  )
where

import Data.List (intercalate)
import Ibis.Syntax.AST.Surface (Literal (..), Pat (..))

-- | De Bruijn index for variables and type variables, relative
-- distance to binder (where 0 is the innermost lambda).
newtype Index = Index {unIndex :: Int}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Enum, Num, Real, Integral)

-- | De Bruijn levels for rigid variables.
newtype Level = Level {unLevel :: Int}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Enum)

-- | Spines are lists of values applied to a rigid or flexible head.
type Spine = [Value]

-- | Closure type for functions in the NbE evaluator.
type Closure = (Value -> Value)

data CoreTerm
  = Universe Int
  | Const String -- Constants (e.g., built-in functions, axioms)
  | MVar String -- Meta-variable for unification
  | Var Index -- De Bruijn indexded variable
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
  -- Language constructs
  | Let Index CoreTerm CoreTerm -- let x = e in body
  | Ann CoreTerm CoreTerm -- e : A
  | Match CoreTerm [(Pat, CoreTerm)]
  | -- Topological primitives
    Site Index -- A topological site
  | Cover CoreTerm CoreTerm -- Cover u v (u ⩿ v)
  | Sect CoreTerm CoreTerm -- Sect A u
  | Res CoreTerm CoreTerm CoreTerm CoreTerm CoreTerm -- res u v a proof site
  | Ext CoreTerm CoreTerm CoreTerm CoreTerm CoreTerm -- ext u v a proof site
  deriving (Eq)

data CoreDecl
  = CoreDef
      { defName :: String -- String retained for diagnostic & symbol linking
      , defType :: CoreTerm
      , defBody :: CoreTerm -- Fully desugared core term
      }
  | CoreInductive
      { indName :: String
      , indType :: CoreTerm
      , indCtors :: [(String, CoreTerm)] -- Constructor names and their types
      }
  deriving (Eq)

data Value
  = VUniverse Int -- Universe levels (types are terms, universes classify types)
  | VConst String -- Constants (e.g., built-in functions, axioms)
  | VLit Literal
  | VCons Value Value -- List constructor
  | VCtor String [Value] -- Data constructor with name and arguments
  | VUnit
  | -- Dependent Functions
    VPi (Maybe String) Value Closure
  | VLam (Maybe String) Value Closure
  | -- Dependent Products
    VSigma (Maybe String) Value Closure
  | VPair Value Value
  | -- Topological primitives
    VSite Index
  | VCover Value Value
  | VSect Value Value
  | -- Stuck
    VNeutral Value Neutral
  | -- Metavariables
    VRigid Level Spine
  | VFlex Int Spine

-- Neutral terms represent computations that are "stuck" on a variable or a neutral term,
-- which cannot be further evaluated.
data Neutral
  = NVar Int
  | NApp Neutral Value
  | NFst Neutral
  | NSnd Neutral
  | NMatch Neutral [(Pat, Value)]
  | NRes Value Value Value Value Neutral
  | NExt Value Value Value Value Neutral

---------------------------------------------
-- SHOW INSTANCES
---------------------------------------------

instance Show CoreTerm where
  show (Universe n) = "Type " ++ show n
  show (Const name) = name
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
  show _ = "<complex term>"

instance Show CoreDecl where
  show (CoreDef name ty body) =
    "def " ++ name ++ " : " ++ show ty ++ " := " ++ show body
  show (CoreInductive name ty ctors) =
    "inductive "
      ++ name
      ++ " : "
      ++ show ty
      ++ " where { "
      ++ intercalate "; " (map (\(ctorName, ctorTy) -> ctorName ++ " : " ++ show ctorTy) ctors)
      ++ " }"

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
newtype CoreProgram = CoreProgram [CoreDecl]
  deriving (Eq, Show)
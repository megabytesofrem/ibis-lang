-- | Core AST for the Ibis programming language.
module Ibis.Syntax.AST.Core
  ( -- * Debruijn indices
    Debrujin

    -- * Core AST
  , Ty (..)
  , Expr (..)
  , Def (..)
  , Program (..)
  )
where

import Ibis.Syntax.AST.Kind (Kind (..))
import Ibis.Syntax.AST.Surface (Literal (..), Pat (..))

-- | Debrujin index for variables and type variables.
type Debrujin = Int

data Ty
  = TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TVar Debrujin Kind -- Type variable, e.g. a : *, f : * -> *
  | TFunc Ty Ty -- Function type, e.g. Int -> Int
  | TForall Kind Ty -- forall a. a -> a
  | TLam Debrujin Ty -- Type-level lambda, e.g. Λ(a:k). T
  | TApp Ty Ty -- Type application, e.g. Maybe Int
  | TCons String [Ty] -- Type constructor with parameters
  deriving (Eq, Show)

data Expr
  = ELit Literal
  | EUnit -- ()
  | EVar Debrujin
  | ELam Ty Expr -- \x -> expr
  | EAbs Expr -- /\a -> expr    (type abstraction)
  | ETyApp Expr Ty -- e [Int]   (type application)
  | EApp Expr Expr -- e₁ e₂      (term application)
  | ELet Ty Expr Expr -- let x : ty = expr1 in expr2
  | EMatch Expr [(Pat, Expr)]
  deriving (Eq, Show)

-- After lowering to Core, we only keep top-level named definitions
-- TODO: Figure out how to handle imports and modules in the core language
data Def = Def
  { defName :: String
  , defType :: Ty
  , defBody :: Expr
  }
  deriving (Eq, Show)

newtype Program = Program [Def]
  deriving (Eq, Show)
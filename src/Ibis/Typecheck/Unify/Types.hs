{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Typecheck.Unify.Types where

import Data.Map qualified as M
import Ibis.Syntax.AST.Core (CoreTerm)

data Equation = Equation
  { eqType :: CoreTerm
  , eqLHS :: CoreTerm
  , eqRHS :: CoreTerm
  }
  deriving (Show, Eq)

data ProblemState
  = Active
  | Blocked
  | Solved
  | Failed String
  deriving (Show, Eq)

data Problem = Problem
  { problemId :: Int
  , problemState :: ProblemState
  , problemEquation :: Equation
  }
  deriving (Show, Eq)

data Elim
  = Apply CoreTerm
  | ProjFst
  | ProjSnd
  deriving (Show, Eq)

data Occurrence
  = OccurStrongRigid
  | OccurFlexRigid
  | OccurNotOccur
  deriving (Show, Eq)

type Type = CoreTerm

newtype MetaVar = MetaVar {unMetaVar :: Int}
  deriving stock (Show, Eq, Ord)

data Entry
  = BVar String CoreTerm -- Bound local variables (from lambda or Pi)
  | BDef String CoreTerm CoreTerm -- x : A = val
  | Meta MetaVar CoreTerm (Maybe CoreTerm) -- ?X : A (or ?X : A = solved_value)
  | Prob Int Problem -- Blocked equations waiting on unsolved meta-variables

type Subst = M.Map MetaVar CoreTerm

-- Our local context is a snoc-list of entries
type LocalCtx = [Entry]
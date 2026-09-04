{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Types and data structures for the unification solver, including metavariables, equations,
-- and the solver state.
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

{- FOURMOLU_DISABLE -}
-- An occurence strategy for a metavariable within a term
data Occurrence
  = OccurStrongRigid -- The meta variable occurs in a rigid position (e.g., applied to a constructor)
  | OccurFlexRigid   -- The meta variable occurs in a flexible position (e.g., applied to a variable)
  | OccurNotOccur    -- The meta variable does not occur in the term
  deriving (Show, Eq)
{- FOURMOLU_ENABLE -}

-- The type of terms we are working with in the unification solver
type Type = CoreTerm

newtype MetaVar = MetaVar {unMetaVar :: Int}
  deriving stock (Show, Eq, Ord)

data Entry
  = BVar String CoreTerm -- Bound local variables (from lambda or Pi)
  | BDef String CoreTerm CoreTerm -- x : A = val
  | Meta MetaVar CoreTerm (Maybe CoreTerm) -- ?X : A (or ?X : A = solved_value)
  | Prob Int Problem -- Blocked equations waiting on unsolved meta-variables
  deriving (Show, Eq)

-- | Substitution mapping from metavariables to terms
--
-- Implementation note: The original paper "A tutorial implementation of dynamic pattern uniﬁcation"
-- by Dale Miller and Gopalan Nadathur uses a list of pairs for substitutions, but we use a Map for efficiency.
type Subst = M.Map MetaVar CoreTerm

data Zip = Zip
  { leftScope :: [Entry] -- Entries to the left of the focus (newer/local)
  , focus :: Maybe Entry -- Currently focused entry (the cursor)
  , rightScope :: [Entry] -- Entries to the right of the focus (older/global)
  }
  deriving (Show, Eq)

-- | The state of the unification solver, including the current scope stack, meta-variable substitution map,
-- and the worklist of problems to solve.
--
-- Implementation note: The original paper "A tutorial implementation of dynamic pattern uniﬁcation"
-- by Dale Miller and Gopalan Nadathur uses a list of problems for the worklist, but we use a record instead.
data SolverState = SolverState
  { context :: Zip
  , metaSubst :: M.Map MetaVar CoreTerm
  , worklist :: M.Map Int Problem
  , nextMetaId :: Int
  , nextProblemId :: Int
  }
  deriving (Show, Eq)
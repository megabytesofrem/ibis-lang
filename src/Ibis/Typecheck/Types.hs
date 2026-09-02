{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- |
  Module      : Ibis.Typecheck.Types
  Description : Types used in the typechecking phase, including elaboration.
-}
module Ibis.Typecheck.Types
  ( -- * Scope helpers
    LocalScope
  , ArityEnv

    -- * Context and state
  , ElabCtx (..)
  , ElabState (..)
  , Level (..)
  , Spine
  , MetaVar (..)
  , MetaVarMap
  , emptyElabCtx
  , emptyElabState

    -- * Elaboration monad
  , ElabM (..)
  , runElaboration

    -- * Context operations
  , lookupName
  , extendCtx
  )
where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.State (MonadState, StateT, runStateT)
import Data.IntMap.Strict qualified as IM
import Data.List (elemIndex)
import Data.Map.Strict qualified as M

import Ibis.Syntax.AST.Core (CoreTerm, Index (..), Level (..), Spine, Value)
import Ibis.Typecheck.Error (TcError (..))

-------------------------------------------------------------
-- SCOPE
-------------------------------------------------------------

-- | A stack of bound variable names; the innermost binder is at the head.
type LocalScope = [String]

-- | Mapping from function names to their expected arities.
type ArityEnv = [(String, Int)]

-------------------------------------------------------------
-- CONTEXT AND STATE
-------------------------------------------------------------

data ElabCtx = ElabCtx
  { scope :: LocalScope -- Current local scope for variable bindings
  }
  deriving (Show, Eq)

data ElabState = ElabState
  { universeMap :: M.Map String Int -- Mapping from universe names to their levels
  , metavars :: IM.IntMap MetaVar -- Metavariable substitution map
  , nextLevel :: Int -- Next available universe level for named universes
  , nextMetaVarId :: Int -- Next available metavariable ID
  }
  deriving (Show, Eq)

emptyElabCtx :: ElabCtx
emptyElabCtx = ElabCtx{scope = []}

emptyElabState :: ElabState
emptyElabState =
  ElabState
    { universeMap = M.empty
    , metavars = IM.empty
    , nextLevel = 1 -- Universe 0 is reserved for Prop
    , nextMetaVarId = 0
    }

-- | A metavariable is either an unsolved hole (carrying the local scope it was
-- created in and its expected type) or a solved term.
data MetaVar
  = Hole LocalScope CoreTerm
  | Solved CoreTerm
  deriving (Show, Eq)

-- | The global metavariable substitution map.
type MetaVarMap = IM.IntMap MetaVar

-------------------------------------------------------------
-- ELABORATION MONAD
-------------------------------------------------------------

-- | The elaboration monad: a read-only scope context layered over mutable
-- elaboration state layered over error propagation.
newtype ElabM a = ElabM {runElabM :: ReaderT ElabCtx (StateT ElabState (Either TcError)) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader ElabCtx
    , MonadState ElabState
    , MonadError TcError
    )

-- | Run the elaboration monad from the empty initial state.
runElaboration :: ElabM a -> ElabCtx -> Either TcError (a, ElabState)
runElaboration elab ctx = runStateT (runReaderT (runElabM elab) ctx) emptyElabState

-------------------------------------------------------------
-- CONTEXT OPERATIONS
-------------------------------------------------------------

lookupName :: String -> ElabM Int
lookupName name = do
  ctx <- ask
  case elemIndex name (scope ctx) of
    Just depth -> pure (length (scope ctx) - depth - 1)
    Nothing -> throwError $ UnboundVariable name

extendCtx :: String -> ElabM a -> ElabM a
extendCtx name = local (\ctx -> ctx{scope = name : scope ctx})

indexToLevel :: Index -> Level -> Level
indexToLevel (Index idx) (Level depth) = Level (depth - idx - 1)

levelToIndex :: Level -> Level -> Index
levelToIndex (Level depth) (Level level) = Index (depth - level - 1)
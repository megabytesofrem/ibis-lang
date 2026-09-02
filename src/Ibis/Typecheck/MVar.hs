{-# LANGUAGE ImportQualifiedPost #-}

{- |
  Module      : Ibis.Typecheck.MVar
  Description : Metavariable resolution and arity-filling operations.
-}
module Ibis.Typecheck.MVar
  ( -- * Metavariable operations
    freshMetaVar
  , solveMetaVar

    -- * Universe level assignment
  , getOrAssignUniverse

    -- * Arity and application helpers
  , checkUnapplied
  , fillMissingParams
  , collectApplications
  )
where

import Control.Monad (replicateM)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M

import Ibis.Syntax.AST.Core (CoreTerm)
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Syntax.AST.Surface (Term (..))
import Ibis.Typecheck.Error (TcError (..))
import Ibis.Typecheck.Types

-------------------------------------------------------------
-- METAVARIABLE OPERATIONS
-------------------------------------------------------------

-- | Allocate a fresh metavariable hole with the given expected type.
-- Returns the numeric ID of the new hole.
freshMetaVar :: CoreTerm -> ElabM Int
freshMetaVar holety = do
  ctx <- ask
  st <- get
  let mvar = nextMetaVarId st
      hole = Hole (scope ctx) holety
  put st{metavars = IM.insert mvar hole (metavars st), nextMetaVarId = mvar + 1}
  pure mvar

-- | Mark a metavariable as solved. Errors if it is already solved or absent.
solveMetaVar :: Int -> CoreTerm -> ElabM ()
solveMetaVar mvar term = do
  st <- get
  case IM.lookup mvar (metavars st) of
    Just (Hole _ _) -> put st{metavars = IM.insert mvar (Solved term) (metavars st)}
    Just (Solved _) -> throwError $ MetavarSolved mvar
    Nothing -> throwError $ MetavarNotFound mvar

-------------------------------------------------------------
-- UNIVERSE LEVEL ASSIGNMENT
-------------------------------------------------------------

-- | Resolve a named universe to its level, assigning a fresh level if this
-- name has not been seen before.
getOrAssignUniverse :: String -> ElabM Int
getOrAssignUniverse name = do
  st <- get
  case M.lookup name (universeMap st) of
    Just level -> pure level
    Nothing -> do
      let newLevel = nextLevel st
      put st{universeMap = M.insert name newLevel (universeMap st), nextLevel = newLevel + 1}
      pure newLevel

-------------------------------------------------------------
-- ARITY AND APPLICATION HELPERS
-------------------------------------------------------------

-- | Check for any under-applied function calls in a surface term.
checkUnapplied :: ArityEnv -> Term -> Either TcError ()
checkUnapplied arityEnv term = case collectApplications term of
  (Var name, args) -> do
    mapM_ (checkUnapplied arityEnv) args
    case lookup name arityEnv of
      Just expectedArity ->
        let actualArity = length args
         in if actualArity < expectedArity
              then Left $ ArityMismatch name expectedArity actualArity
              else Right ()
      Nothing -> Right ()
  (headTerm, args) -> do
    mapM_ (checkUnapplied arityEnv) args
    checkUnapplied arityEnv headTerm

-- | Fill missing parameters in a surface term with fresh metavariables
-- for the unifier to solve later on.
fillMissingParams :: ArityEnv -> Term -> ElabM CoreTerm
fillMissingParams aenv term = case collectApplications term of
  (Var name, args) -> case lookup name aenv of
    Just expectedArity -> do
      coreArgs <- mapM (fillMissingParams aenv) args
      dbIndex <- lookupName name
      if length coreArgs < expectedArity
        then do
          let missingCount = expectedArity - length coreArgs
          newMetas <- replicateM missingCount (freshMetaVar (Core.Universe 0))
          let metaTerms = map (\mId -> Core.MVar ("m" ++ show mId)) newMetas
          pure $ foldl Core.App (Core.Var dbIndex) (coreArgs ++ metaTerms)
        else pure $ foldl Core.App (Core.Var dbIndex) coreArgs
    Nothing -> do
      dbIndex <- lookupName name
      coreArgs <- mapM (fillMissingParams aenv) args
      pure $ foldl Core.App (Core.Var dbIndex) coreArgs
  (headTerm, args) -> do
    coreHead <- fillMissingParams aenv headTerm
    coreArgs <- mapM (fillMissingParams aenv) args
    pure $ foldl Core.App coreHead coreArgs

-- | Decompose a left-spine of applications: @f x y@ → @(f, [x, y])@.
collectApplications :: Term -> (Term, [Term])
collectApplications = go []
 where
  go args (App f x) = go (x : args) f
  go args t = (t, args)

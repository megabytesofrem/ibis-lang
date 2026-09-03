{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Unification and meta-variable solving pass based on Miller's Higher Order
-- Pattern Unification algorithm.
module Ibis.Typecheck.Unify.Solver where

import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT, get, put)

import Data.Map qualified as M

import Control.Monad (when)
import Ibis.Syntax.AST.Core (CoreTerm)
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Typecheck.Error (TcError (..))
import Ibis.Typecheck.Unify.HasFMV (HasFMV (..))
import Ibis.Typecheck.Unify.Types

-- The reduction monad: unifies and solves meta-variables, threading the local context through
newtype Reduce a = Reduce {runReduce :: StateT LocalCtx (ExceptT TcError IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState LocalCtx
    , MonadError TcError
    , MonadIO
    )

-- Stashing functions
------------------------------------------------

stashActive :: Int -> Equation -> Reduce ()
stashActive probId eq = do
  ctx <- get
  put (Prob probId (Problem probId Active eq) : ctx)

stashBlocked :: Int -> Equation -> Reduce ()
stashBlocked probId eq = do
  ctx <- get
  put (Prob probId (Problem probId Blocked eq) : ctx)

stashFailed :: Int -> String -> Equation -> Reduce ()
stashFailed probId msg eq = do
  ctx <- get
  put (Prob probId (Problem probId (Failed msg) eq) : ctx)

stashSolved :: Int -> Equation -> Reduce ()
stashSolved probId eq = do
  ctx <- get
  put (Prob probId (Problem probId Solved eq) : ctx)

stashSimplified :: Int -> Problem -> [Problem] -> Reduce ()
stashSimplified probId simplifiedProblem newProblems = undefined

-- Context manipulation functions

popL :: Reduce Entry
popL = do
  ctx <- get
  case ctx of
    [] -> throwError $ Other "Local context is empty, cannot pop."
    (e : es) -> put es >> pure e

popR :: Reduce (Maybe (Either Subst Entry))
popR = do
  ctx <- get
  case ctx of
    [] -> pure Nothing
    (Meta m _ (Just solved) : es) -> do
      put es
      -- Return the substitution for the solved meta-variable
      pure $ Just (Left (M.singleton m solved))
    (e : es) -> do
      put es
      -- Return the popped entry, as-is
      pure $ Just (Right e)

pushL :: Entry -> Reduce ()
pushL entry = do
  ctx <- get
  put (entry : ctx)

pushR :: Either Subst Entry -> Reduce ()
pushR (Left subst) = do
  ctx <- get
  -- Apply the substitution to the local context
  let newCtx = applySubst subst ctx
  put newCtx
pushR (Right entry) = do
  ctx <- get
  put (entry : ctx)

applySubst :: Subst -> LocalCtx -> LocalCtx
applySubst subst = undefined

defineMeta :: MetaVar -> Type -> CoreTerm -> Reduce ()
defineMeta targetMeta ty solvedVal = undefined

lookupMeta :: MetaVar -> Reduce (Maybe CoreTerm)
lookupMeta targetM = do
  ctx <- get
  pure $ lookupInCtx ctx
 where
  -- Metavariable does not exist in the context
  lookupInCtx [] = Nothing
  -- Lookup
  lookupInCtx (entry : es) = case entry of
    -- Check the current entry for the target metavariable
    Meta m _ mval | m == targetM -> case mval of
      Just solved -> Just solved -- Found and solved
      Nothing -> Nothing -- Found but unsolved, hole
    _ -> lookupInCtx es

--

unroll :: CoreTerm -> Maybe (Int, [Elim])
unroll term = go term []
 where
  go (Core.MVar m) es = Just (m, es)
  go (Core.App f e) es = go f (Apply e : es)
  go (Core.Fst p) es = go p (ProjFst : es)
  go (Core.Snd p) es = go p (ProjSnd : es)
  go _ _ = Nothing

occurence :: [MetaVar] -> CoreTerm -> Occurrence
occurence metas (Core.MVar m) =
  -- If the metavariable is in the list of metas, it's a strong rigid occurrence
  if MetaVar m `elem` metas
    then OccurStrongRigid
    else OccurNotOccur
occurence metas _ = error "Occurrence check not implemented for this term type."

isStrongRigid :: Occurrence -> Bool
isStrongRigid OccurStrongRigid = True
isStrongRigid _ = False

invert :: MetaVar -> Type -> [Elim] -> CoreTerm -> Reduce (Maybe CoreTerm)
invert targetMeta tty es rhs = do
  let occ = occurence [targetMeta] rhs
  when (isStrongRigid occ) $
    throwError $
      Other "Occurs check failed: target meta variable occurs in the right-hand side."

  pure Nothing

tryInvert :: Int -> Equation -> Type -> Reduce () -> Reduce ()
tryInvert p eq@(Equation eqTy lhs rhs) tty cont = case unroll lhs of
  Just (alpha, es) -> do
    inv <- invert (MetaVar alpha) tty es rhs
    case inv of
      Just solvedVal -> do
        stashActive p eq
        defineMeta (MetaVar alpha) eqTy solvedVal
      Nothing -> cont
  Nothing -> cont

rigidRigid :: CoreTerm -> CoreTerm -> Reduce [Equation]
rigidRigid (Core.Universe u1) (Core.Universe u2) = do
  if u1 == u2
    then pure []
    else throwError $ Other "Universe levels do not match."
rigidRigid _ _ =
  throwError $
    Other "Rigid-rigid unification failed: terms do not match."

flexRigid :: [Entry] -> Int -> Problem -> Reduce ()
flexRigid skipped p problem@(Problem targetMeta spine rhs) = do
  entry <- popL
  case entry of
    Meta beta ty Nothing
      -- Occurs check: target meta variable is a free variable in the skipped entries, cannot solve
      | targetMeta == (unMetaVar beta) && beta `elem` (fmv skipped) -> do
          pushL entry
          mapM_ pushL skipped
          stashBlocked p (problemEquation problem)
      -- Target meta variable located
      | targetMeta == (unMetaVar beta) -> do
          mapM_ pushL skipped
          tryInvert p (problemEquation problem) ty $ do
            stashBlocked p (problemEquation problem)
      | beta `elem` (fmv problem) || beta `elem` (fmv skipped) -> do
          -- Retain entry in skipped stack and continue searching
          flexRigid (entry : skipped) p problem
    _ -> do
      pushR (Right entry)
      flexRigid skipped p problem

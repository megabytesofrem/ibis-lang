{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Unification and meta-variable solving pass based on Miller's Higher Order
-- Pattern Unification algorithm.
module Ibis.Typecheck.Unify.Solver where

import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT, get, modify, put)

import Data.Map qualified as M

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Ibis.Syntax.AST.Core (CoreTerm (..), Index)
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Typecheck.Error (TcError (..))
import Ibis.Typecheck.Unify.HasFMV (HasFMV (..))
import Ibis.Typecheck.Unify.Types

-- The reduction monad: unifies and solves meta-variables, threading the local context through
newtype Reduce a = Reduce {runReduce :: StateT SolverState (ExceptT TcError IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState SolverState
    , MonadError TcError
    , MonadIO
    )

-- Stashing functions
------------------------------------------------

stash :: Int -> ProblemState -> Equation -> Reduce ()
stash probId state eq = modify $ \ctx -> ctx{worklist = M.insert probId (Problem probId state eq) (worklist ctx)}

stashActive :: Int -> Equation -> Reduce ()
stashActive probId eq = stash probId Active eq

stashBlocked :: Int -> Equation -> Reduce ()
stashBlocked probId eq = stash probId Blocked eq

stashFailed :: Int -> String -> Equation -> Reduce ()
stashFailed probId msg eq = stash probId (Failed msg) eq

stashSolved :: Int -> Equation -> Reduce ()
stashSolved probId eq = stash probId Solved eq

stashSimplified :: Int -> Problem -> [Problem] -> Reduce ()
stashSimplified _probId simplifiedProblem newProblems = do
  st <- get
  let wl1 = M.insert (problemId simplifiedProblem) simplifiedProblem (worklist st)
      wl2 = foldr (\p acc -> M.insert (problemId p) p acc) wl1 newProblems
  put st{worklist = wl2}

-- Context manipulation functions
-------------------------------------------------

defineMeta :: MetaVar -> Type -> CoreTerm -> Reduce ()
defineMeta targetMeta ty solvedVal = do
  st <- get
  let subst = M.singleton targetMeta solvedVal
      st' = applySubst subst st

  -- Update the metaSubst mapping with the new definition
  put st'{metaSubst = M.insert targetMeta solvedVal (metaSubst st')}

lookupMeta :: MetaVar -> Reduce (Maybe CoreTerm)
lookupMeta targetM = do
  st <- get
  pure $ M.lookup targetM (metaSubst st)

applySubst :: Subst -> SolverState -> SolverState
applySubst subst st =
  st
    { scopeStack = map updateEnv (scopeStack st)
    , worklist = M.map updateProblem (worklist st)
    }
 where
  updateEnv :: Entry -> Entry
  updateEnv entry = case entry of
    BVar name ty -> BVar name (substTerm subst ty)
    BDef name ty val -> BDef name (substTerm subst ty) (substTerm subst val)
    Meta m ty maybeVal -> Meta m (substTerm subst ty) (fmap (substTerm subst) maybeVal)
    Prob pid prob -> Prob pid (updateProblem prob)

  updateProblem :: Problem -> Problem
  updateProblem p = p{problemEquation = updateEquation (problemEquation p)}

  updateEquation :: Equation -> Equation
  updateEquation eq =
    Equation
      (substTerm subst (eqType eq))
      (substTerm subst (eqLHS eq))
      (substTerm subst (eqRHS eq))

  substTerm :: Subst -> CoreTerm -> CoreTerm
  substTerm s t = case t of
    Core.MVar m -> fromMaybe t (M.lookup (MetaVar m) s)
    _ -> t

-- Occurence checking and inversion functions
------------------------------------------------

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

-- Check if a metavariable occurs in a term (used for occurs check during unification)
occursIn :: MetaVar -> CoreTerm -> Bool
occursIn targetMeta (Core.MVar m) = targetMeta == MetaVar m
occursIn targetMeta (Core.App f arg) = occursIn targetMeta f || occursIn targetMeta arg
occursIn targetMeta (Core.Lam _ body) = occursIn targetMeta body
occursIn targetMeta (Core.Pi _ dom cod) = occursIn targetMeta dom || occursIn targetMeta cod
occursIn targetMeta (Core.Sigma _ dom cod) = occursIn targetMeta dom || occursIn targetMeta cod
occursIn targetMeta (Core.Pair a b) = occursIn targetMeta a || occursIn targetMeta b
occursIn targetMeta (Core.Fst p) = occursIn targetMeta p
occursIn targetMeta (Core.Snd p) = occursIn targetMeta p
occursIn targetMeta (Core.Let _ e body) = occursIn targetMeta e || occursIn targetMeta body
occursIn _ _ = False

invert :: MetaVar -> Type -> [CoreTerm] -> CoreTerm -> Reduce CoreTerm
invert targetMeta tty es rhs = do
  -- Occurs check: does the meta variable occur in the right-hand side of the equation?
  let occ = occurence [targetMeta] rhs
  when (isStrongRigid occ) $
    throwError $
      Other "Occurs check failed: target meta variable occurs in the right-hand side."

  case buildSpineMap es of
    Just varMap -> do
      -- Rename the right-hand side according to the variable mapping
      case invertScope varMap rhs of
        Just renamedRhs -> pure renamedRhs
        Nothing -> throwError $ Other "Cannot invert: failed to rename scope."
    Nothing -> throwError $ Other "Cannot invert: spine is not a valid pattern."

-- Build a mapping from the spine of metavariable applications to local parameter indices
--
-- In a higher-order unification problem (?M x_0 x_1 .. x_n = rhs), the spine
-- (x_0, x_1, ..., x_n) must satisfy the Miller pattern condition: each variable in the spine
-- must be distinct and must not occur in the right-hand side (rhs) of the equation
--
-- In practice, this functionm extracts and builds the inverted spine mapping from @args@:
-- such that:
--    Outer variable index (in rhs) -> Local parameter index (in ?m's lambda body)
--
-- Fail conditions:
-- 1. If any variable in the spine is repeated (i.e., not distinct)
-- 2. If the spine contains any complex term (i.e., not a variable)
buildSpineMap :: [CoreTerm] -> Maybe [(Index, Index)]
buildSpineMap args = go args 0 []
 where
  go [] _ acc = Just acc
  go (Core.Var x : xs) newIdx acc
    | x `notElem` map fst acc = go xs (newIdx + 1) ((x, newIdx) : acc)
    | otherwise = Nothing -- Variable repeated; fails pattern condition
  go _ _ _ = Nothing -- Complex term in spine; fails pattern condition

-- Invert the scope of the right hand side term according to the variable mapping
--
-- In practice, this function replaces outer-scope variables in the rhs with their
-- corresponding local parameter indices based on the provided mapping @varMap@, constructed from
-- @extractPattern@.
invertScope :: [(Index, Index)] -> CoreTerm -> Maybe CoreTerm
invertScope varMap term = case term of
  Core.Var x -> Core.Var <$> lookup x varMap
  Core.Pi name dom cod -> Core.Pi name <$> invertScope varMap dom <*> invertScope varMap cod
  Core.Lam name body -> Core.Lam name <$> invertScope varMap body
  Core.App f arg -> Core.App <$> invertScope varMap f <*> invertScope varMap arg
  Core.Sigma name dom cod -> Core.Sigma name <$> invertScope varMap dom <*> invertScope varMap cod
  Core.Pair a b -> Core.Pair <$> invertScope varMap a <*> invertScope varMap b
  Core.Fst p -> Core.Fst <$> invertScope varMap p
  Core.Snd p -> Core.Snd <$> invertScope varMap p
  Core.Let name e body -> Core.Let name <$> invertScope varMap e <*> invertScope varMap body
  _ -> Just term -- For other terms, return as-is

unwindApp :: CoreTerm -> (CoreTerm, [CoreTerm])
unwindApp (Core.App f arg) = let (g, args) = unwindApp f in (g, args ++ [arg])
unwindApp t = (t, [])

tryInvert :: Int -> Equation -> Type -> Reduce () -> Reduce ()
tryInvert p eq@(Equation _eqTy lhs rhs) tty cont = case unwind lhs of
  Just (alpha, es) -> do
    inv <- invert (MetaVar alpha) tty es rhs
    stashActive p eq
    defineMeta (MetaVar alpha) tty inv
  Nothing -> cont
 where
  -- Unwind the left-hand side of the equation to extract a pair of (metavariable, spine)
  -- if it is of the form ?X e1 e2 ... en.
  unwind :: CoreTerm -> Maybe (Int, [CoreTerm])
  unwind term = case unwindApp term of
    (Core.MVar m, args) -> Just (m, args)
    _ -> Nothing

-- Handle rigid-rigid unification
--
-- This is the case where both sides of the equation are rigid terms (i.e., not metavariables).
rigidRigid :: CoreTerm -> CoreTerm -> Reduce [Equation]
rigidRigid (Core.Universe u1) (Core.Universe u2) = do
  if u1 == u2
    then pure []
    else throwError $ Other "Universe levels do not match."
rigidRigid (Core.Pi _ a1 b1) (Core.Pi _ a2 b2) = do
  pure [Equation a1 a1 a2, Equation b1 b1 b2]
rigidRigid (Core.Sigma _ a1 b1) (Core.Sigma _ a2 b2) = do
  pure [Equation a1 a1 a2, Equation b1 b1 b2]
rigidRigid _ _ =
  throwError $
    Other "Unification failed: terms do not match."

-- Handle flexible-rigid unification
--
-- This is the case where the left-hand side is a metavariable applied to some arguments,
-- and the right-hand side is a rigid term.
flexRigid :: Int -> Problem -> Reduce ()
flexRigid p prob@(Problem targetMeta _ _rhs) = do
  let eq = problemEquation prob
      lhs = eqLHS eq
      _tty = eqType eq
  case unwindApp lhs of
    (Core.MVar m, es) -> do
      let meta = MetaVar m
      let targetMeta' = MetaVar targetMeta

      st <- get
      let (afterMeta, rest) = break (\case Meta mv _ _ -> mv == meta; _ -> False) (scopeStack st)
      case rest of
        (Meta _ ty _ : _) -> do
          -- Scope check: does the target metavariable appear in the free variables of later bindings?
          if targetMeta' `elem` (fmv afterMeta)
            then stashBlocked p eq
            else tryInvert p eq ty (stashBlocked p eq)
        _ ->
          -- Metavariable is either already solved (in metaBindings) or missing
          stashBlocked p eq
    _ -> throwError $ Other "Unification failed: lhs is not a metavariable application."
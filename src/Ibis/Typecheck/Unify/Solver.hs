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
import Ibis.Syntax.AST.Core (CoreTerm (..), Index (Index))
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Typecheck.Error (TcError (..))
import Ibis.Typecheck.Unify.HasFMV (HasFMV (..), freeVars)
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

modifyContext :: (Zip -> (a, Zip)) -> Reduce a
modifyContext f = do
  st <- get
  let (res, ctx') = f (context st)
  put st{context = ctx'}
  pure res

pushL :: Entry -> Reduce ()
pushL e = modify $ \st ->
  let ctx = context st
   in st{context = ctx{leftScope = e : leftScope ctx}}

pushR :: Entry -> Reduce ()
pushR e = modify $ \st ->
  let ctx = context st
   in st{context = ctx{rightScope = e : rightScope ctx}}

popL :: Reduce (Maybe Entry)
popL = modifyContext $ \ctx -> case ctx of
  Zip (e : lefts) focus' rights -> (Just e, Zip lefts focus' rights)
  Zip [] focus' rights -> (Nothing, Zip [] focus' rights)

setFocus :: Maybe Entry -> Zip -> Zip
setFocus f z = z{focus = f}

popR :: Zip -> (Maybe Entry, Zip)
popR z = case rightScope z of
  [] -> (Nothing, z)
  (e : es) -> (Just e, z{rightScope = es})

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
    { context = updateContext (context st)
    , worklist = M.map updateProblem (worklist st)
    }
 where
  updateContext :: Zip -> Zip
  updateContext z =
    Zip
      { leftScope = map updateEnv (leftScope z)
      , focus = fmap updateEnv (focus z)
      , rightScope = map updateEnv (rightScope z)
      }

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

freshMetaId :: Reduce Int
freshMetaId = do
  st <- get
  let fresh = nextMetaId st
  put st{nextMetaId = fresh + 1}
  pure fresh

freshProblemId :: Reduce Int
freshProblemId = do
  st <- get
  let fresh = nextMetaId st
  put st{nextMetaId = fresh + 1}
  pure fresh

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

-- | Given a metavariable @targetMeta@, a type @tty@, a list of arguments @es@, and a right-hand side term @rhs@,
-- invert attempts to find a value for the metavariable that satisfies the equation ?M es = rhs, if possible.
--
-- This may throw an error up the monad stack if the inversion is not possible, either
-- due to the Miller pattern condition not being satisfied or due to an occurs check failure.
invert :: MetaVar -> Type -> [CoreTerm] -> CoreTerm -> Reduce CoreTerm
invert targetMeta _tty es rhs = do
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

-- Unwind an application term into its head and argument list
unwindApp :: CoreTerm -> (CoreTerm, [CoreTerm])
unwindApp (Core.App f arg) = let (g, args) = unwindApp f in (g, args ++ [arg])
unwindApp t = (t, [])

-- | Build a mapping from the spine of metavariable applications to local parameter indices
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
    -- If x is not already in the mapping, add it with the new index and recurse
    | x `notElem` map fst acc = go xs (newIdx + 1) ((x, newIdx) : acc)
    | otherwise = Nothing -- Variable repeated; fails pattern condition
  go _ _ _ = Nothing -- Complex term in spine; fails pattern condition

-- | Invert the scope of the right hand side term according to the variable mapping
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

-- | Given two spines (lists of De Bruijn indices) @xs@ and @ys@ applied to the same
-- metavariable (?M xs =? ?M ys), this function computes the intersection of the two spines.
-- It keeps the common variable scope where both argument lists agree (x == y) and drops any
-- parameters where they disagree.
--
-- How it works:
--
-- 1. Inductive Case:
--    - Recursively steps down the Pi-types of the metavariable while walking both argument spines.
--    - Assigns a fresh De Bruijn index for each parameter position.
--    - Always logs the parameter into @phi@ (which tracks all original scope parameters).
--    - Logs the parameter into @psi@ ONLY if the arguments at the current position match (x == y).
--    - Substitutes the fresh index into the codomain and recurses on the remaining arguments.
--
-- 2. Base Case (when spines are fully traversed):
--    - Verifies that the return type term @s@ does not depend on any variables that were dropped.
--      (i.e., @s@ must only depend on the retained variables tracked in @psi@).
--    - If @s@ depends on a dropped variable, returns 'Nothing' because scope pruning is impossible.
--    - Otherwise, builds a pruned Pi-type accepting only the retained parameters in @psi@.
--    - Constructs a lambda wrapper @mkSolution@ that takes a fresh, restricted metavariable hole,
--      applies the retained @psi@ arguments to it, and wraps it in lambdas over all original @phi@
--      parameters so it plugs cleanly back into the original call site.
intersect
  :: [(Index, Type)]
  -- ^ @phi@: The original scope parameters (all parameters from the original Pi-type)
  -> [(Index, Type)]
  -- ^ @psi@: The retained scope parameters (only those where the spines agree)
  -> Type
  -- ^ The return type term @s@ of the Pi-type
  -> [Index]
  -- ^ The left-hand side spine (arguments applied to the metavariable)
  -> [Index]
  -- ^ The right-hand side spine (arguments applied to the metavariable)
  -> Reduce (Maybe (Type, CoreTerm -> CoreTerm))
  -- ^ Returns a pruned Pi-type and a solution constructor if intersection is successful, or 'Nothing' if not.
intersect phi psi s [] [] = do
  -- Find all free variables in the right-hand side term s
  let fvs = freeVars s
  -- Retain only those variables from psi that are in the free variables of s
  let retained = map fst psi

  -- Scope check: Does the right-hand side term s only depend on retained variables?
  if all (`elem` retained) fvs
    then do
      -- Prune the Pi type by keeping only the variables that are in the free variables of s
      let prunedPi = foldr (\(_, ty) acc -> Core.Pi (Just "x") ty acc) s psi

      -- Construct the solution term for the metavariable as a lambda abstraction over the pruned variables
      -- using only retained variables from psi, and applying them to the right-hand side term s.
      let mkSolution beta =
            let innerApp = foldl' Core.App beta (map (Core.Var . fst) psi)
             in foldr (\(_, _ty) acc -> Core.Lam (Just "x") acc) innerApp phi -- ty is not used in the lambda
      pure $ Just (prunedPi, mkSolution)
    else pure Nothing

-- Inductive case: If both spines are non-empty, we recursively step down the Pi type
-- We check if the heads of the spines match, and if so, we recurse on the tails of the spines.
intersect phi psi (Core.Pi name dom cod) (x : xs) (y : ys) = do
  let freshIdx = Index (length phi)

  -- let Ψ' = Ψ ++ if x == y then [(z, A)] else []

  -- If the current indices x and y match, we add the fresh index and its type to psi; otherwise, we only add it to phi.
  let psi' = if x == y then psi ++ [(freshIdx, dom)] else psi

  -- We always add the fresh index and its type to phi, which tracks all original scope parameters.
  let phi' = phi ++ [(freshIdx, dom)]

  -- Recursively intersect the codomain with the updated contexts
  let cod' = substIndex freshIdx (Core.Var x) cod
  intersect phi' psi' cod' xs ys
 where
  substIndex :: Index -> CoreTerm -> CoreTerm -> CoreTerm
  substIndex idx replacement term = undefined

-- If the spine lengths do not match, we cannot intersect the contexts; return Nothing
intersect _ _ _ _ _ = pure Nothing

-- Try to intersect two spines
--
-- This function checks if both argument types @ds@ and @es@ are valid spines (i.e., they consist purely)
-- of metavariable indices.
--
-- If they are valid spines, it attempts to intersect them using the @intersect@ function, calculating
-- a pruned type for a fresh hole and solves the equation by defining the metavariable with a solution term constructed
-- from the pruned Pi type and the right-hand side term.
--
-- If not (if either intersection fails, or the spines are not valid), it stashes the problem as blocked
-- on the worklist for later processing.
tryIntersect
  :: Int
  -- ^ Problem ID
  -> Equation
  -- ^ The active equation to solve
  -> MetaVar
  -- ^ The target metavariable α to solve for
  -> Type
  -- ^ The type of the metavariable α
  -> [CoreTerm]
  -- ^ The left-hand side spine (arguments applied to α)
  -> [CoreTerm]
  -- ^ The right-hand side spine (arguments applied to α)
  -> Reduce ()
tryIntersect p eq@(Equation _eqTy lhs rhs) targetMeta tty ds es = do
  case (toVars ds, toVars es) of
    (Just xs, Just ys) -> do
      -- Attempt to intersect the two spines
      res <- intersect [] [] tty xs ys
      case res of
        -- If intersection is successful, we get a pruned Pi type and a solution constructor
        Just (prunedPi, mkSolution) -> do
          -- Allocate a fresh ID for the restricted hole
          betaId <- freshMetaId
          let betaMeta = MetaVar betaId
          pushL (Meta betaMeta prunedPi Nothing)

          -- Define the meta-variable with the solution term constructed from the pruned Pi type and the right-hand side term
          defineMeta targetMeta tty (mkSolution (Core.MVar betaId))

          -- Stash the problem as solved
          stashSolved p eq

        -- If intersection fails, we stash the problem as blocked
        Nothing -> stashBlocked p eq

    -- If either spine contains non-variable terms, we cannot intersect; stash as blocked
    _ -> stashBlocked p eq
 where
  toVars :: [CoreTerm] -> Maybe [Index]
  toVars = mapM extractVar

  extractVar :: CoreTerm -> Maybe Index
  extractVar (Core.Var x) = Just x
  extractVar _ = Nothing

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
    (Core.MVar m, _es) -> do
      let meta = MetaVar m
      let targetMeta' = MetaVar targetMeta

      -- Walk left context using popL until we focus the meta
      popL >>= \case
        Just e@(Meta mv ty _)
          | mv == meta -> do
              -- Scope check using elements remaining on left stack
              st <- get
              if targetMeta' `elem` fmv (leftScope (context st))
                then do
                  _ <- pushL e
                  stashBlocked p eq
                else tryInvert p eq ty (stashBlocked p eq)
          | otherwise -> do
              _ <- pushR e
              flexRigid p prob
        Just e -> do
          _ <- pushR e
          flexRigid p prob
        Nothing ->
          stashBlocked p eq
    _ -> throwError $ Other "Unification failed: lhs is not a metavariable application."

flexFlex :: Int -> Problem -> Reduce ()
flexFlex p prob@(Problem targetMeta _ _rhs) = undefined

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

    -- * Unification
  , unify
  )
where

import Control.Monad (foldM, replicateM, zipWithM_)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M

import Ibis.Syntax.AST.Core (CoreTerm, Index (..))
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Syntax.AST.Surface (Pat (..), Term (..))
import Ibis.Typecheck.Error (TcError (..))
import Ibis.Typecheck.Eval (applyVal, eval)
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
          pure $ foldl' Core.App (Core.Var . Index $ dbIndex) (coreArgs ++ metaTerms)
        else pure $ foldl' Core.App (Core.Var . Index $ dbIndex) coreArgs
    Nothing -> do
      dbIndex <- lookupName name
      coreArgs <- mapM (fillMissingParams aenv) args
      pure $ foldl' Core.App (Core.Var . Index $ dbIndex) coreArgs
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

-------------------------------------------------------------
-- MILLER PATTERN UNIFICATION
-------------------------------------------------------------

-- | Force a value to weak head normal form.
--
-- The solver currently keeps metavariables explicit, so forcing is a no-op
-- until a solved-meta dereference layer is added. Keeping this hook avoids a
-- recursive dependency on normalization code and keeps the unifier local.
force :: Core.Value -> ElabM Core.Value
force val = case val of
  Core.VFlex mvar spine -> do
    st <- get
    case IM.lookup mvar (metavars st) of
      Just (Solved coreTerm) -> do
        let valSol = eval [] coreTerm
        applied <- foldM applyOne valSol spine
        force applied
      _ -> pure val
  _ -> pure val
 where
  applyOne :: Core.Value -> Core.Value -> ElabM Core.Value
  applyOne acc arg = case applyVal acc arg of
    Right next -> pure next
    Left msg -> throwError $ Other msg

-- | Extract the head rigid variable index and its application spine.
-- Only plain variable-headed spines are admissible Miller patterns.
collectRigidSpine :: Core.Value -> Maybe (Int, Spine)
collectRigidSpine = go
 where
  go (Core.VNeutral _ (Core.NVar idx)) = Just (idx, [])
  go (Core.VRigid (Level idx) []) = Just (idx, [])
  go _ = Nothing

-- | Verify that a spine is a Miller pattern spine.
-- Returns a map from rigid variable index to the position of that variable in
-- the spine, which is later used to abstract the solution over those variables.
checkPattern :: Level -> Spine -> ElabM (IM.IntMap Int)
checkPattern level spine = go IM.empty 0 spine
 where
  go :: IM.IntMap Int -> Int -> Spine -> ElabM (IM.IntMap Int)
  go acc _ [] = pure acc
  go acc position (arg : rest) = do
    arg' <- force arg
    case collectRigidSpine arg' of
      Just (idx, [])
        | idx >= unLevel level -> throwError $ Other "Miller pattern variable is out of scope"
        | IM.member idx acc -> throwError $ Other "Non-linear Miller pattern spine"
        | otherwise -> go (IM.insert idx position acc) (position + 1) rest
      _ -> throwError $ Other "Metavariable spine must be a sequence of distinct rigid variables"

-- | Occurs check pass for metavariable solving.
occursCheckMeta :: Level -> Int -> Core.Value -> ElabM ()
occursCheckMeta currentLvl targetMeta = go currentLvl
 where
  go :: Level -> Core.Value -> ElabM ()
  go lvl value = do
    value' <- force value
    case value' of
      Core.VFlex m spine
        | m == targetMeta -> throwError $ Other $ "Occurs check failed for ?m" ++ show targetMeta
        | otherwise -> mapM_ (go lvl) spine
      Core.VRigid _ spine -> mapM_ (go lvl) spine
      Core.VNeutral ty neu -> go lvl ty >> goNeutral lvl neu
      Core.VPi _ dom cod -> do
        go lvl dom
        let fresh = Core.VRigid lvl []
        go (Level (unLevel lvl + 1)) (cod fresh)
      Core.VLam _ dom body -> do
        go lvl dom
        let fresh = Core.VRigid lvl []
        go (Level (unLevel lvl + 1)) (body fresh)
      Core.VSigma _ dom cod -> do
        go lvl dom
        let fresh = Core.VRigid lvl []
        go (Level (unLevel lvl + 1)) (cod fresh)
      Core.VPair a b -> go lvl a >> go lvl b
      Core.VCons a b -> go lvl a >> go lvl b
      Core.VCtor _ args -> mapM_ (go lvl) args
      Core.VCover a b -> go lvl a >> go lvl b
      Core.VSect a b -> go lvl a >> go lvl b
      Core.VUniverse _ -> pure ()
      Core.VConst _ -> pure ()
      Core.VLit _ -> pure ()
      Core.VUnit -> pure ()
      Core.VSite _ -> pure ()

  goNeutral :: Level -> Core.Neutral -> ElabM ()
  goNeutral lvl neu = case neu of
    Core.NVar _ -> pure ()
    Core.NApp n x -> goNeutral lvl n >> go lvl x
    Core.NFst n -> goNeutral lvl n
    Core.NSnd n -> goNeutral lvl n
    Core.NMatch n branches -> goNeutral lvl n >> mapM_ (go lvl . snd) branches
    Core.NRes a b c d n -> go lvl a >> go lvl b >> go lvl c >> go lvl d >> goNeutral lvl n
    Core.NExt a b c d n -> go lvl a >> go lvl b >> go lvl c >> go lvl d >> goNeutral lvl n

-- | Scope-check pass for metavariable solving.
scopeCheckMeta :: Level -> IM.IntMap Int -> Core.Value -> ElabM ()
scopeCheckMeta currentLvl renamed = go currentLvl
 where
  go :: Level -> Core.Value -> ElabM ()
  go lvl value = do
    value' <- force value
    case value' of
      -- Trivial cases: no need to check scope for these values
      Core.VUniverse _ -> pure ()
      Core.VConst _ -> pure ()
      Core.VLit _ -> pure ()
      Core.VUnit -> pure ()
      Core.VSite _ -> pure ()
      Core.VFlex _ spine -> mapM_ (go lvl) spine
      Core.VRigid (Level idx) spine -> do
        -- A variable is valid if it's in the spine map OR bound locally after solving started
        if IM.member idx renamed || idx >= unLevel currentLvl
          then mapM_ (go lvl) spine
          else throwError $ Other $ "Scope check failed: variable " ++ show idx ++ " out of scope for metavariable"
      Core.VPi _ dom cod -> do
        go lvl dom
        let fresh = Core.VRigid lvl []
        -- Recursively check the codomain with an incremented level for the bound variable
        go (Level (unLevel lvl + 1)) (cod fresh)
      Core.VLam _ dom body -> do
        go lvl dom
        let fresh = Core.VRigid lvl []
        -- Recursively check the body with an incremented level for the bound variable
        go (Level (unLevel lvl + 1)) (body fresh)
      Core.VSigma _ dom cod -> do
        go lvl dom
        let fresh = Core.VRigid lvl []
        -- Recursively check the codomain with an incremented level for the bound variable
        go (Level (unLevel lvl + 1)) (cod fresh)
      Core.VPair a b -> go lvl a >> go lvl b
      Core.VCons a b -> go lvl a >> go lvl b
      Core.VCtor _ args -> mapM_ (go lvl) args
      Core.VCover a b -> go lvl a >> go lvl b
      Core.VSect a b -> go lvl a >> go lvl b
      Core.VNeutral ty neu -> go lvl ty >> goNeutral lvl neu

  goNeutral :: Level -> Core.Neutral -> ElabM ()
  goNeutral lvl neu = case neu of
    Core.NVar idx ->
      if IM.member idx renamed || idx >= unLevel currentLvl
        then pure ()
        else throwError $ Other $ "Scope check failed: variable " ++ show idx ++ " out of scope for metavariable"
    Core.NApp n x -> goNeutral lvl n >> go lvl x
    Core.NFst n -> goNeutral lvl n
    Core.NSnd n -> goNeutral lvl n
    Core.NMatch n branches -> goNeutral lvl n >> mapM_ (go lvl . snd) branches
    Core.NRes a b c d n -> go lvl a >> go lvl b >> go lvl c >> go lvl d >> goNeutral lvl n
    Core.NExt a b c d n -> go lvl a >> go lvl b >> go lvl c >> go lvl d >> goNeutral lvl n

-- |
--   Quote pass for Miller Pattern Unification (Readback & Scope Inversion).
--
--   This function converts a semantic 'Core.Value' back into a syntactic 'CoreTerm'
--   solution for a metavariable ?m, while simultaneously enforcing variable scoping.
--
--   WHAT THIS IS ACTUALLY DOING UNDER THE HOOD:
--
--   1. Spine Inversion / Renaming:
--      - 'renamed' is the inverted spine map (IntMap Int) from 'checkPattern'. It maps a variable's
--        absolute de Bruijn Level (its identity) to its position (0..spineLen-1) in the metavariable's spine.

---      - When encountering a rigid variable (VRigid/NVar idx), we look it up in 'renamed':
--        * Just position -> The variable was in the spine! We map it to a relative
--          de Bruijn Index: Index (spineLen - 1 - position).
--        * Nothing       -> The variable was NOT in the spine, meaning it is out-of-scope
--          for this solution. We throw a scope error.
--
--   2. Normalization & Dereferencing:
--      - Calls 'force' on every value to dereference any metavariables solved in state prior to quoting.
--
--   3. Closure Unrolling:
--      - For binders (VPi, VLam, VSigma), it instantiates the closure body using a fresh
--        VRigid variable at 'currentLvl', increments 'currentLvl', and recursively quotes the body.
--
--   PARAMETERS:
--     * currentLvl : The current evaluation depth / level context.
--     * spineLen   : The total number of arguments in the metavariable's spine (used for index relative math).
--     * ren        : Map from absolute Level -> spine argument position (the inverted spine map).
--     * value      : The right-hand side 'Core.Value' being solved and quoted into a CoreTerm.
quoteMeta :: Level -> Int -> IM.IntMap Int -> Core.Value -> ElabM CoreTerm
quoteMeta currentLvl spineLen renamed value = do
  value' <- force value
  case value' of
    Core.VUniverse n -> pure $ Core.Universe n
    Core.VConst name -> pure $ Core.Const name
    Core.VLit lit -> pure $ Core.Lit lit
    Core.VUnit -> pure Core.Unit
    Core.VSite idx -> pure $ Core.Site idx
    Core.VFlex m spine -> do
      args' <- mapM (quoteMeta currentLvl spineLen renamed) spine
      pure $ foldl Core.App (Core.MVar ("m" ++ show m)) args'
    Core.VRigid (Level idx) spine -> case IM.lookup idx renamed of
      Just position -> do
        args' <- mapM (quoteMeta currentLvl spineLen renamed) spine
        let dbIdx = Index (spineLen - 1 - position)
        pure $ foldl Core.App (Core.Var dbIdx) args'
      Nothing -> throwError $ Other $ "Scope check failed: variable " ++ show idx ++ " out of scope for metavariable"
    Core.VPi name dom cod -> do
      dom' <- quoteMeta currentLvl spineLen renamed dom
      let fresh = Core.VRigid currentLvl []
      cod' <- quoteMeta (Level (unLevel currentLvl + 1)) spineLen renamed (cod fresh)
      pure $ Core.Pi name dom' cod'
    Core.VLam name dom body -> do
      dom' <- quoteMeta currentLvl spineLen renamed dom
      let fresh = Core.VRigid currentLvl []
      body' <- quoteMeta (Level (unLevel currentLvl + 1)) spineLen renamed (body fresh)
      pure $ Core.Lam name body'
    Core.VSigma name dom cod -> do
      dom' <- quoteMeta currentLvl spineLen renamed dom
      let fresh = Core.VRigid currentLvl []
      cod' <- quoteMeta (Level (unLevel currentLvl + 1)) spineLen renamed (cod fresh)
      pure $ Core.Sigma name dom' cod'
    Core.VCtor name args -> do
      args' <- mapM (quoteMeta currentLvl spineLen renamed) args
      pure $ foldl Core.App (Core.Const name) args'
    Core.VPair a b -> Core.Pair <$> quoteMeta currentLvl spineLen renamed a <*> quoteMeta currentLvl spineLen renamed b
    Core.VCons a b -> do
      a' <- quoteMeta currentLvl spineLen renamed a
      b' <- quoteMeta currentLvl spineLen renamed b
      pure $ Core.App (Core.App (Core.Const "cons") a') b'
    Core.VCover a b -> Core.Cover <$> quoteMeta currentLvl spineLen renamed a <*> quoteMeta currentLvl spineLen renamed b
    Core.VSect a b -> Core.Sect <$> quoteMeta currentLvl spineLen renamed a <*> quoteMeta currentLvl spineLen renamed b
    Core.VNeutral _ neu -> quoteNeutralMeta currentLvl spineLen renamed neu

-- |
--   Helper to quote stuck / neutral terms back to CoreTerm syntax.
--   Applies the exact same spine-inversion lookup to head variables (NVar).
quoteNeutralMeta :: Level -> Int -> IM.IntMap Int -> Core.Neutral -> ElabM CoreTerm
quoteNeutralMeta currentLvl spineLen ren neu = case neu of
  Core.NVar idx -> case IM.lookup idx ren of
    Just position -> pure $ Core.Var (Index (spineLen - 1 - position))
    Nothing -> throwError $ Other $ "Scope check failed: variable " ++ show idx ++ " out of scope for metavariable"
  Core.NApp n x -> Core.App <$> quoteNeutralMeta currentLvl spineLen ren n <*> quoteMeta currentLvl spineLen ren x
  Core.NFst n -> Core.Fst <$> quoteNeutralMeta currentLvl spineLen ren n
  Core.NSnd n -> Core.Snd <$> quoteNeutralMeta currentLvl spineLen ren n
  Core.NMatch n branches -> do
    headTerm <- quoteNeutralMeta currentLvl spineLen ren n
    branchTerms <-
      mapM
        ( \(pat, body) -> do
            body' <- quoteMeta (Level (unLevel currentLvl + countPatternVars pat)) spineLen ren body
            pure (pat, body')
        )
        branches
    pure $ Core.Match headTerm branchTerms
  Core.NRes a b c d n ->
    Core.Res
      <$> quoteMeta currentLvl spineLen ren a
      <*> quoteMeta currentLvl spineLen ren b
      <*> quoteMeta currentLvl spineLen ren c
      <*> quoteMeta currentLvl spineLen ren d
      <*> quoteNeutralMeta currentLvl spineLen ren n
  Core.NExt a b c d n ->
    Core.Ext
      <$> quoteMeta currentLvl spineLen ren a
      <*> quoteMeta currentLvl spineLen ren b
      <*> quoteMeta currentLvl spineLen ren c
      <*> quoteMeta currentLvl spineLen ren d
      <*> quoteNeutralMeta currentLvl spineLen ren n

countPatternVars :: Pat -> Int
countPatternVars pat = case pat of
  PLit _ -> 0
  PCapture _ -> 1
  PWildcard -> 0
  PTuple pats -> sum (map countPatternVars pats)
  PCtor _ pats -> sum (map countPatternVars pats)
  PPartition _ inner -> 1 + countPatternVars inner

-- | Solve a metavariable by abstracting over a Miller spine and storing the resulting term.
solve :: Level -> Int -> Spine -> Core.Value -> ElabM ()
solve level mvar spine val = do
  renameMap <- checkPattern level spine
  let spineLen = length spine
  occursCheckMeta level mvar val
  scopeCheckMeta level renameMap val
  rhsTerm <- quoteMeta level spineLen renameMap val
  let solution = foldr (\_ acc -> Core.Lam (Just "x") acc) rhsTerm spine
  solveMetaVar mvar solution

-- | A small convenience wrapper for explicit unification problems.
unify :: Level -> Core.Value -> Core.Value -> ElabM ()
unify level lhs rhs = do
  lhs' <- force lhs
  rhs' <- force rhs
  case (lhs', rhs') of
    (Core.VFlex m spine, other) -> solve level m spine other
    (other, Core.VFlex m spine) -> solve level m spine other
    (Core.VUniverse a, Core.VUniverse b)
      | a == b -> pure ()
      | otherwise -> throwError $ Other "Universe-universe unification failed"
    (Core.VConst a, Core.VConst b)
      | a == b -> pure ()
      | otherwise -> throwError $ Other "Const-const unification failed"
    (Core.VLit a, Core.VLit b)
      | a == b -> pure ()
      | otherwise -> throwError $ Other "Literal-literal unification failed"
    (Core.VUnit, Core.VUnit) -> pure ()
    (Core.VSite a, Core.VSite b)
      | a == b -> pure ()
      | otherwise -> throwError $ Other "Site-site unification failed"
    (Core.VPi _ dom1 cod1, Core.VPi _ dom2 cod2) -> do
      unify level dom1 dom2
      let fresh = Core.VRigid level []
      unify (Level (unLevel level + 1)) (cod1 fresh) (cod2 fresh)
    (Core.VLam _ _ body1, Core.VLam _ _ body2) -> do
      let fresh = Core.VRigid level []
      unify (Level (unLevel level + 1)) (body1 fresh) (body2 fresh)
    (Core.VSigma _ dom1 cod1, Core.VSigma _ dom2 cod2) -> do
      unify level dom1 dom2
      let fresh = Core.VRigid level []
      unify (Level (unLevel level + 1)) (cod1 fresh) (cod2 fresh)
    (Core.VPair a1 b1, Core.VPair a2 b2) -> unify level a1 a2 >> unify level b1 b2
    (Core.VCons a1 b1, Core.VCons a2 b2) -> unify level a1 a2 >> unify level b1 b2
    (Core.VCtor n1 as1, Core.VCtor n2 as2)
      | n1 == n2 && length as1 == length as2 -> zipWithM_ (unify level) as1 as2
      | otherwise -> throwError $ Other "Constructor-constructor unification failed"
    (Core.VCover a1 b1, Core.VCover a2 b2) -> unify level a1 a2 >> unify level b1 b2
    (Core.VSect a1 b1, Core.VSect a2 b2) -> unify level a1 a2 >> unify level b1 b2
    (Core.VRigid lvl1 spine1, Core.VRigid lvl2 spine2)
      | lvl1 == lvl2 && length spine1 == length spine2 -> zipWithM_ (unify level) spine1 spine2
      | otherwise -> throwError $ Other "Rigid-rigid unification failed"
    (Core.VNeutral ty1 neu1, Core.VNeutral ty2 neu2) ->
      unify level ty1 ty2 >> unifyNeutral level neu1 neu2
    _ -> throwError $ Other "Unification failed"

unifyNeutral :: Level -> Core.Neutral -> Core.Neutral -> ElabM ()
unifyNeutral level n1 n2 = case (n1, n2) of
  (Core.NVar a, Core.NVar b)
    | a == b -> pure ()
    | otherwise -> throwError $ Other "Neutral variable mismatch"
  (Core.NApp f1 x1, Core.NApp f2 x2) -> unifyNeutral level f1 f2 >> unify level x1 x2
  (Core.NFst x1, Core.NFst x2) -> unifyNeutral level x1 x2
  (Core.NSnd x1, Core.NSnd x2) -> unifyNeutral level x1 x2
  (Core.NMatch n1' bs1, Core.NMatch n2' bs2)
    | length bs1 == length bs2 -> do
        unifyNeutral level n1' n2'
        unifyBranches level bs1 bs2
    | otherwise -> throwError $ Other "Neutral match branch-count mismatch"
  (Core.NRes a1 b1 c1 d1 n1', Core.NRes a2 b2 c2 d2 n2') -> do
    unify level a1 a2
    unify level b1 b2
    unify level c1 c2
    unify level d1 d2
    unifyNeutral level n1' n2'
  (Core.NExt a1 b1 c1 d1 n1', Core.NExt a2 b2 c2 d2 n2') -> do
    unify level a1 a2
    unify level b1 b2
    unify level c1 c2
    unify level d1 d2
    unifyNeutral level n1' n2'
  _ -> throwError $ Other "Neutral-neutral unification failed"

unifyBranches :: Level -> [(Pat, Core.Value)] -> [(Pat, Core.Value)] -> ElabM ()
unifyBranches level bs1 bs2 =
  zipWithM_ unifyBranch bs1 bs2
 where
  unifyBranch :: (Pat, Core.Value) -> (Pat, Core.Value) -> ElabM ()
  unifyBranch (pat1, body1) (pat2, body2)
    | pat1 == pat2 = unify (Level (unLevel level + countPatternVars pat1)) body1 body2
    | otherwise = throwError $ Other "Neutral match pattern mismatch"
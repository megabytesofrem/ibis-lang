{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- |
  Module      : Ibis.Syntax.Elaborate
  Description : Elaborates surface syntax into core syntax and desugars syntactic sugar.
-}
module Ibis.Syntax.Elaborate where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)
import Data.List (elemIndex)
import Data.Map.Strict qualified as M

import Ibis.Syntax.AST (Binop (..), Literal (LitBool), Param (..), Unop (..))
import Ibis.Syntax.AST.Core (CoreDecl (..), CoreTerm)
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Syntax.AST.Surface (Decl (..), Pat (..), Term (..))

data ElabCtx = ElabCtx
  { nameMap :: M.Map String Int -- Mapping from variable names to De Bruijn indices
  , currentDepth :: Int -- Current depth in the context for De Bruijn indices
  }
  deriving (Show, Eq)

emptyElabCtx :: ElabCtx
emptyElabCtx = ElabCtx M.empty 0

-- | Elaboration monad used for elaborating surface syntax into core syntax
newtype ElabM a = ElabM {runElabM :: ReaderT ElabCtx (Either String) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader ElabCtx
    , MonadError String
    )

lookupName :: String -> ElabM Int
lookupName name = do
  ctx <- ask
  case M.lookup name (nameMap ctx) of
    Just depth -> pure (currentDepth ctx - depth - 1) -- Convert to De Bruijn index
    Nothing -> throwError $ "Unbound variable: " ++ name

extendCtx :: String -> ElabM a -> ElabM a
extendCtx name action = local updateCtx action
 where
  updateCtx (ElabCtx nmap depth) =
    ElabCtx (M.insert name depth nmap) (depth + 1)

-- Extract variable bindings from patterns
extractPatternVars :: Pat -> [String]
extractPatternVars (PLit _) = []
extractPatternVars (PCapture name) = [name]
extractPatternVars PWildcard = []
extractPatternVars (PTuple pats) = concatMap extractPatternVars pats
extractPatternVars (PCtor _ pats) = concatMap extractPatternVars pats
extractPatternVars (PPartition _ pat) = extractPatternVars pat

-- Desugar for expressions to an application of a monadic map function
-- For example, `for x in collection do body` is desugared to `mapM (\x -> body) collection`
desugarFor :: String -> Term -> Term -> ElabM CoreTerm
desugarFor var collection body = do
  elabCollection <- elabTerm collection
  extendCtx var $ do
    elabBody <- elabTerm body
    pure $
      Core.App
        ( Core.App
            (Core.Const "mapM")
            (Core.Lam (Just var) elabBody)
        )
        elabCollection

-- | Elaborate a surface term into a core term
elabTerm :: Term -> ElabM CoreTerm
elabTerm (Universe n) = pure $ Core.Universe n
elabTerm (Var name) = do
  idx <- lookupName name
  pure $ Core.Var idx
elabTerm (Lit lit) = pure $ Core.Lit lit
elabTerm (Pi name ty body) = do
  elabTy <- elabTerm ty
  extendCtx name $ do
    elabBody <- elabTerm body
    pure $ Core.Pi (Just name) elabTy elabBody
elabTerm (Lam name body) = do
  extendCtx name $ do
    elabBody <- elabTerm body
    pure $ Core.Lam (Just name) elabBody
elabTerm (App f x) = do
  elabF <- elabTerm f
  elabX <- elabTerm x
  pure $ Core.App elabF elabX
elabTerm (Sigma name ty body) = do
  elabTy <- elabTerm ty
  extendCtx name $ do
    elabBody <- elabTerm body
    pure $ Core.Sigma (Just name) elabTy elabBody
elabTerm (Pair a b) = do
  elabA <- elabTerm a
  elabB <- elabTerm b
  pure $ Core.Pair elabA elabB
elabTerm (Fst p) = do
  elabP <- elabTerm p
  pure $ Core.Fst elabP
elabTerm (Snd p) = do
  elabP <- elabTerm p
  pure $ Core.Snd elabP
elabTerm (Let name mTy e body) = do
  elabE <- elabTerm e
  case mTy of
    Just ty -> do
      _elabTy <- elabTerm ty
      extendCtx name $ do
        elabBody <- elabTerm body
        pure $ Core.Let 0 elabE elabBody -- Note: De Bruijn index for the bound variable is 0
    Nothing -> do
      extendCtx name $ do
        elabBody <- elabTerm body
        pure $ Core.Let 0 elabE elabBody -- Note: De Bruijn index for the bound variable is 0
elabTerm (Unop op e) = do
  elabE <- elabTerm e
  pure $ Core.App (Core.Const (unopToString op)) elabE
elabTerm (Binop op e1 e2) = do
  elabE1 <- elabTerm e1
  elabE2 <- elabTerm e2
  pure $ Core.App (Core.App (Core.Const (binopToString op)) elabE1) elabE2
elabTerm (If cond thenBranch elseBranch) = do
  elabCond <- elabTerm cond
  elabThen <- elabTerm thenBranch
  elabElse <- elabTerm elseBranch
  pure $
    Core.Match
      elabCond
      [ (PLit (LitBool True), elabThen)
      , (PLit (LitBool False), elabElse)
      ]
elabTerm (For var collection body) = desugarFor var collection body
elabTerm (Match scrutinee branches) = do
  elabScrutinee <- elabTerm scrutinee
  elabBranches <- mapM elabBranch branches
  pure $ Core.Match elabScrutinee elabBranches
 where
  elabBranch (pat, body) = do
    -- Extend context based on pattern variables
    let patVars = extractPatternVars pat
    extendCtxs patVars $ do
      elabBody <- elabTerm body
      pure (pat, elabBody)

  extendCtxs [] action = action
  extendCtxs (v : vs) action = extendCtx v (extendCtxs vs action)
---
--
elabTerm tm = throwError $ "Elaboration not implemented for term: " ++ show tm

elabDecl :: Decl -> ElabM [CoreDecl]
elabDecl (TermDecl term) = do
  elabTerm' <- elabTerm term
  pure $ [Core.CoreDef "<anonymous>" elabTerm' elabTerm']
elabDecl (StructDecl sname params fields) = elabStruct sname params fields
--

elabDecl _ = throwError "Elaboration for this declaration type is not implemented yet."

-- Elaborate a structure into an inductive type
elabStruct :: String -> [Param] -> [(String, Term)] -> ElabM [CoreDecl]
elabStruct sname params fields = do
  elabFieldTys <- mapM (elabTerm . snd) fields

  -- Build the canonical constructor name
  let ctorName = sname ++ "_mk"
  let targetTy = foldl' Core.App (Core.Const sname) (map (const $ Core.Var 0) params)

  -- Construct the constructor type as a Pi type over the fields: Field1Ty -> Field2Ty -> ... -> StructType
  let ctorSig = foldr (Core.Pi Nothing) targetTy elabFieldTys

  let indDecl =
        Core.CoreInductive
          { Core.indName = sname
          , Core.indType = targetTy
          , Core.indCtors = [(ctorName, ctorSig)]
          }

  projections <- mapM (\(fname, _) -> genProjection sname fname fields) fields

  pure (indDecl : projections)

elabInductive :: String -> [(String, Term)] -> ElabM CoreDecl
elabInductive name ctors = do
  elabCtors <-
    mapM
      ( \(ctorName, ctorTy) -> do
          elabCtorTy <- elabTerm ctorTy
          pure (ctorName, elabCtorTy)
      )
      ctors

  let indTy = Core.Const name
  pure $
    Core.CoreInductive
      { Core.indName = name
      , Core.indType = indTy
      , Core.indCtors = elabCtors
      }

-- | Generate a projection function for a given field of a structure
genProjection
  :: String
  -- ^ Structure name
  -> String
  -- ^ Field name
  -> [(String, Term)]
  -- ^ List of fields with their types
  -> ElabM CoreDecl
genProjection sname fieldName fields = do
  -- Get the index of the target field and it's corresponding type
  (targetIdx, fTy) <- case fieldName `elemIndex` (map fst fields) of
    Just idx -> pure (idx, snd (fields !! idx))
    Nothing -> throwError $ "Field " ++ fieldName ++ " not found in structure " ++ sname

  elabFTy <- elabTerm fTy
  let projName = sname ++ "_" ++ fieldName
  let ctorName = sname ++ "_mk"

  let projType = Core.Pi (Just "self") (Core.Const sname) elabFTy

  let patVars = [PCapture ("v" ++ show i) | (i, _) <- zip [(0 :: Integer) ..] fields]
      ctorPat = PCtor ctorName patVars

  let targetVar = Core.Var (length fields - targetIdx - 1)
  let body =
        Core.Lam (Just "self") $
          Core.Match (Core.Var 0) [(ctorPat, targetVar)]

  pure $ Core.CoreDef projName projType body

unopToString :: Unop -> String
unopToString = show

binopToString :: Binop -> String
binopToString = show

runElaboration :: ElabM a -> ElabCtx -> Either String a
runElaboration elab ctx = runReaderT (runElabM elab) ctx
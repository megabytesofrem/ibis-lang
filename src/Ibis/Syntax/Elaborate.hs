{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- |
  Module      : Ibis.Syntax.Elaborate
  Description : Elaborates surface syntax into core syntax and desugars syntactic sugar.
-}
module Ibis.Syntax.Elaborate where

import Control.Monad.Except (MonadError, throwError)
import Data.List (elemIndex)
import Data.Map.Strict qualified as M

import Control.Monad.State (MonadState, StateT, get, put, runStateT)
import Ibis.Syntax.AST (Binop (..), Literal (LitBool), Param (..), SurfaceUniverse (..), Unop (..))
import Ibis.Syntax.AST.Core (CoreDecl (..), CoreTerm)
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Syntax.AST.Surface (Decl (..), Pat (..), Term (..))

data ElabCtx = ElabCtx
  { nameMap :: M.Map String Int -- Mapping from variable names to De Bruijn indices
  , currentDepth :: Int -- Current depth in the context for De Bruijn indices
  , universeMap :: M.Map String Int -- Mapping from universe names to their levels
  , nextLevel :: Int -- Next available universe level for fresh universes
  }
  deriving (Show, Eq)

emptyElabCtx :: ElabCtx
emptyElabCtx =
  ElabCtx
    { nameMap = M.empty
    , currentDepth = 0
    , universeMap = M.empty
    , nextLevel = 1 -- Universes start from 1, as 0 is reserved for Prop
    }

-- | Elaboration monad used for elaborating surface syntax into core syntax
newtype ElabM a = ElabM {runElabM :: StateT ElabCtx (Either String) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState ElabCtx
    , MonadError String
    )

lookupName :: String -> ElabM Int
lookupName name = do
  ctx <- get
  case M.lookup name (nameMap ctx) of
    Just depth -> pure (currentDepth ctx - depth - 1) -- Convert to De Bruijn index
    Nothing -> throwError $ "Unbound variable: " ++ name

localState :: (MonadState s m) => (s -> s) -> m a -> m a
localState f action = do
  orig <- get
  put (f orig)
  result <- action
  put orig
  pure result

extendCtx :: String -> ElabM a -> ElabM a
extendCtx name action = localState updateCtx action
 where
  updateCtx (ElabCtx nmap depth uMap next) =
    ElabCtx
      (M.insert name depth nmap)
      (depth + 1)
      uMap
      next

-- | Get the universe level for a given universe name, assigning a new level if it doesn't exist
getOrAssignUniverse :: String -> ElabM Int
getOrAssignUniverse name = do
  ctx <- get
  case M.lookup name (universeMap ctx) of
    Just level -> pure level
    Nothing -> do
      let newLevel = nextLevel ctx
      put $
        ctx
          { universeMap = M.insert name newLevel (universeMap ctx)
          , nextLevel = newLevel + 1
          }
      pure newLevel

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

-- Desugar a monadic do block to a chain of bind operations. For example,
-- `do { x <- e1; e2 }` is desugared to `e1 >>= (\x -> e2)`.
desugarMonadicDo :: [Term] -> ElabM CoreTerm
desugarMonadicDo [] = throwError "Empty do block"
desugarMonadicDo [term] = elabTerm term
desugarMonadicDo (Bind name expr : rest) = do
  elabExpr <- elabTerm expr
  elabRest <- extendCtx name $ desugarMonadicDo rest
  pure $
    Core.App
      (Core.App (Core.Const ">>=") elabExpr)
      (Core.Lam (Just name) elabRest)
desugarMonadicDo (_ : _) = throwError "Invalid do block"

elabUniverse :: SurfaceUniverse -> ElabM CoreTerm
elabUniverse (UnivName name) = do
  level <- getOrAssignUniverse name
  pure $ Core.Universe level
elabUniverse (UnivLevel level) = pure $ Core.Universe level

-- | Elaborate a surface term into a core term
elabTerm :: Term -> ElabM CoreTerm
elabTerm tm = case tm of
  Universe univ -> elabUniverse univ
  Const name -> pure $ Core.Const name
  MVar name -> pure $ Core.MVar name
  Var name -> do
    idx <- lookupName name
    pure $ Core.Var idx
  Lit lit -> pure $ Core.Lit lit
  Unit -> pure Core.Unit
  Pi name ty body -> do
    elabTy <- elabTerm ty
    extendCtx name $ do
      elabBody <- elabTerm body
      pure $ Core.Pi (Just name) elabTy elabBody
  Lam name body -> do
    extendCtx name $ do
      elabBody <- elabTerm body
      pure $ Core.Lam (Just name) elabBody
  App f x -> do
    elabF <- elabTerm f
    elabX <- elabTerm x
    pure $ Core.App elabF elabX
  Sigma name ty body -> do
    elabTy <- elabTerm ty
    extendCtx name $ do
      elabBody <- elabTerm body
      pure $ Core.Sigma (Just name) elabTy elabBody
  Pair a b -> do
    elabA <- elabTerm a
    elabB <- elabTerm b
    pure $ Core.Pair elabA elabB
  Fst p -> do
    elabP <- elabTerm p
    pure $ Core.Fst elabP
  Snd p -> do
    elabP <- elabTerm p
    pure $ Core.Snd elabP
  Let name mTy e body -> do
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
  Ann e ty -> do
    elabE <- elabTerm e
    elabTy <- elabTerm ty
    pure $ Core.Ann elabE elabTy
  Unop op e -> do
    elabE <- elabTerm e
    pure $ Core.App (Core.Const (unopToString op)) elabE
  Binop op e1 e2 -> do
    elabE1 <- elabTerm e1
    elabE2 <- elabTerm e2
    pure $ Core.App (Core.App (Core.Const (binopToString op)) elabE1) elabE2
  List elems -> do
    elabElems <- mapM elabTerm elems
    pure $
      foldr
        (\e acc -> Core.App (Core.App (Core.Const "cons") e) acc)
        (Core.Const "nil")
        elabElems
  If cond thenBranch elseBranch -> do
    elabCond <- elabTerm cond
    elabThen <- elabTerm thenBranch
    elabElse <- elabTerm elseBranch
    pure $
      Core.Match
        elabCond
        [ (PLit (LitBool True), elabThen)
        , (PLit (LitBool False), elabElse)
        ]
  For var collection body -> desugarFor var collection body
  Match scrutinee branches -> do
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
  --
  -- Monadic constructs
  Do terms -> desugarMonadicDo terms
  Bind _name _expr -> throwError "Bind should only appear inside a do block"
  --
  -- Topological Presheaf Primitives
  Site name -> do
    -- TODO: should this use lookupName or its own map?
    idx <- lookupName name
    pure $ Core.Site idx
  Cover u v -> do
    elabU <- elabTerm u
    elabV <- elabTerm v
    pure $ Core.Cover elabU elabV
  Sect a u -> do
    elabA <- elabTerm a
    elabU <- elabTerm u
    pure $ Core.Sect elabA elabU

  -- TODO: Figure out how to synthesize a proof term for Ext/Res
  Res u v -> throwError "Res elaboration not implemented yet"
  Ext a u v -> throwError "Ext elaboration not implemented yet"

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

-- | Run the elaboration monad with a given context
runElaboration :: ElabM a -> ElabCtx -> Either String (a, ElabCtx)
runElaboration elab ctx = runStateT (runElabM elab) ctx
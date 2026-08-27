{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | AST lowering pass from the surface AST to the core AST.
module Ibis.LowerToCore
  ( LowerM
  , LowerCtx (..)
  , mkLowerCtx
  , defaultLowerCtx
  , runLowerToCore
  )
where

import Data.List (elemIndex)

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader (..))

import Ibis.Syntax.AST.Core (Debrujin)
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Syntax.AST.Kind (Kind (..))
import Ibis.Syntax.AST.Surface (Pat (..))
import Ibis.Syntax.AST.Surface qualified as Surf

-- | Context used during lowering to keep track of terms and types
data LowerCtx = LowerCtx
  { termEnv :: [String]
  , typeEnv :: [String]
  }
  deriving (Show, Eq)

-- | LowerM monad used during the lowering pass, which carries the context and can throw errors
-- "bubbling" them up the call stack.
newtype LowerM a = LowerM {unLowerM :: ReaderT LowerCtx (Either String) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader LowerCtx
    , MonadError String
    )

mkLowerCtx :: LowerCtx
mkLowerCtx = LowerCtx{termEnv = [], typeEnv = []}

defaultLowerCtx :: LowerCtx
defaultLowerCtx =
  LowerCtx
    { termEnv = ["Cons", "Nil", "True", "False"]
    , typeEnv = []
    }

lookupTermM :: String -> LowerM Debrujin
lookupTermM name = do
  ctx <- ask
  case name `elemIndex` termEnv ctx of
    Just idx -> pure idx
    Nothing -> throwError $ "Unbound term variable: " <> name

lookupTypeM :: String -> LowerM Debrujin
lookupTypeM name = do
  ctx <- ask
  case name `elemIndex` typeEnv ctx of
    Just idx -> pure idx
    Nothing -> throwError $ "Unbound type variable: " <> name

-- LOWERING

-- | Run the lowering pass, lowering the surface AST to the core AST.
-- This function gets passed the initial context for the lowering pass.
runLowerToCore :: LowerCtx -> Surf.Program -> Either String Core.Program
runLowerToCore ctx ast = runReaderT (unLowerM (lowerToCore ast)) ctx

-- | Lower the surface AST to the core AST
lowerToCore :: Surf.Program -> LowerM Core.Program
lowerToCore (Surf.Program decls) = do
  loweredDecls <- traverse lowerDecl decls
  pure $ Core.Program (concat loweredDecls)

-- | Lower a type from the surface AST to the core AST
lowerTy :: Surf.Ty -> LowerM Core.Ty
lowerTy Surf.TInt = pure Core.TInt
lowerTy Surf.TFloat = pure Core.TFloat
lowerTy Surf.TBool = pure Core.TBool
lowerTy Surf.TString = pure Core.TString
lowerTy Surf.TUnit = pure Core.TUnit
lowerTy (Surf.TVar name mkind) = do
  idx <- lookupTypeM name

  -- If the kind is not provided, default to KStar.
  let kind = case mkind of
        Just k -> k
        Nothing -> KStar

  pure $ Core.TVar idx kind
lowerTy (Surf.TFunc arg ret) = do
  loweredArg <- lowerTy arg
  loweredRet <- lowerTy ret
  pure $ Core.TFunc loweredArg loweredRet
lowerTy (Surf.TForall name kind ty) = do
  let newCtx = LowerCtx{termEnv = termEnv mkLowerCtx, typeEnv = name : typeEnv mkLowerCtx}
  loweredTy <- local (const newCtx) (lowerTy ty)
  pure $ Core.TForall kind loweredTy
lowerTy (Surf.TApp t1 t2) = do
  loweredT1 <- lowerTy t1
  loweredT2 <- lowerTy t2
  pure $ Core.TApp loweredT1 loweredT2
lowerTy (Surf.TCons name params) = do
  loweredParams <- traverse lowerTy params
  pure $ Core.TCons name loweredParams

-- | Desugar list expressions into nested application of Cons and Nil.
-- For example, [1, 2, 3] becomes Cons 1 (Cons 2 (Cons 3 Nil)).
desugarList :: [Surf.Expr] -> LowerM Core.Expr
desugarList = aux
 where
  aux [] = do
    idx <- lookupTermM "Nil"
    pure $ Core.EVar idx
  aux (e : es) = do
    loweredHead <- lowerExpr e
    loweredTail <- aux es
    idx <- lookupTermM "Cons"
    pure $ Core.EApp (Core.EApp (Core.EVar idx) loweredHead) loweredTail

desugarTuple :: [Surf.Expr] -> LowerM Core.Expr
desugarTuple = desugarList

-- | Lower an expression from the surface AST to the core AST
lowerExpr :: Surf.Expr -> LowerM Core.Expr
lowerExpr = \case
  Surf.ELit lit -> pure $ Core.ELit lit
  Surf.EUnit -> pure Core.EUnit
  Surf.EVar name -> do
    idx <- lookupTermM name
    pure $ Core.EVar idx
  Surf.EUnop op e -> do
    idx <- lookupTermM (show op)
    loweredE <- lowerExpr e
    pure $ Core.EApp (Core.EVar idx) loweredE
  Surf.EBinop op lhs rhs -> do
    idx <- lookupTermM (show op)
    loweredLhs <- lowerExpr lhs
    loweredRhs <- lowerExpr rhs
    pure $ Core.EApp (Core.EApp (Core.EVar idx) loweredLhs) loweredRhs
  Surf.EList xs -> do
    -- Lists are desugared into nested applications of Cons and Nil.
    desugarList xs
  Surf.ETuple xs -> do
    -- Tuples are desugared into nil/cons-like structures, same as lists
    desugarTuple xs
  Surf.EApp e1 e2 -> do
    loweredE1 <- lowerExpr e1
    loweredE2 <- lowerExpr e2
    pure $ Core.EApp loweredE1 loweredE2
  Surf.ELet (Surf.Binder name mty) value body -> do
    loweredTy <- maybe (pure Core.TUnit) lowerTy mty
    loweredE1 <- lowerExpr value
    -- Extend the context with the new variable for the body of the let expression.
    loweredBody <- local (\ctx -> ctx{termEnv = name : termEnv ctx}) (lowerExpr body)
    pure $ Core.ELet loweredTy loweredE1 loweredBody
  Surf.EIf cond thenExpr elseExpr -> do
    loweredCond <- lowerExpr cond
    loweredThen <- lowerExpr thenExpr
    loweredElse <- lowerExpr elseExpr
    -- Desugar if-then-else into a match expression
    let branches =
          [ (PLit (Surf.LitBool True), loweredThen) -- True branch
          , (PLit (Surf.LitBool False), loweredElse) -- False branch
          ]
    pure $ Core.EMatch loweredCond branches
  Surf.EMatch expr branches -> do
    loweredExpr <- lowerExpr expr
    -- Lower each branch, extending the context with variables bound by the pattern.
    loweredBranches <- traverse lowerBranch branches
    pure $ Core.EMatch loweredExpr loweredBranches

  -- Not implemented yet: type abstractions, type applications, etc.
  _ -> throwError "Lowering of this expression type is not implemented yet"

lowerDecl :: Surf.Decl -> LowerM [Core.Def]
lowerDecl = \case
  Surf.ExprDecl e -> do
    loweredExpr <- lowerExpr e
    pure [Core.Def "_expr" Core.TUnit loweredExpr]

  -- Not implemented yet: function declarations, type declarations, etc.
  _ -> throwError "Lowering of this declaration type is not implemented yet"

extractPatternVars :: Surf.Pat -> [String]
extractPatternVars (PLit _) = []
extractPatternVars (PCapture name) = [name]
extractPatternVars (PTuple pats) = concatMap extractPatternVars pats
extractPatternVars (PList pats) = concatMap extractPatternVars pats
extractPatternVars (PCtor _ pats) = concatMap extractPatternVars pats
extractPatternVars (PPartition hName tailPat) =
  extractPatternVars (PCapture hName) ++ extractPatternVars tailPat
extractPatternVars PWildcard = []

lowerBranch :: (Surf.Pat, Surf.Expr) -> LowerM (Surf.Pat, Core.Expr)
lowerBranch (pat, expr) = do
  let newVars = extractPatternVars pat
  let newCtx = LowerCtx{termEnv = newVars ++ termEnv mkLowerCtx, typeEnv = typeEnv mkLowerCtx}
  loweredExpr <- local (const newCtx) (lowerExpr expr)
  pure (pat, loweredExpr)
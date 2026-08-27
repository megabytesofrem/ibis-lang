{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Ibis.Typecheck.Typecheck where

import Control.Monad.Except (MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader (..), runReaderT)

import Control.Monad (forM, forM_, unless)

import Ibis.Syntax.AST.Core (Debrujin)
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Syntax.AST.Surface qualified as AST
import Ibis.Typecheck.Environment (TypecheckEnv (..), TypecheckM (..))

-- | Typechecker monad (defined in Ibis.Typecheck.Environment)
type Typechecker = TypecheckM TypecheckEnv

-- | Lookup a type variable by its de Bruijn index in the type environment
lookupType :: Debrujin -> Typechecker Core.Ty
lookupType idx = do
  env <- ask
  if idx < length (typeEnv env)
    then return $ typeEnv env !! idx
    else throwError $ "Unbound type variable: " ++ show idx

-- | Lookup a term variable by its name in the term environment
lookupTerm :: String -> Typechecker Core.Ty
lookupTerm name = do
  env <- ask
  case lookup name (termEnv env) of
    Just ty -> return ty
    Nothing -> throwError $ "Unbound term variable: " ++ name

-- | Lookup a term variable by its de Bruijn index in the term environment
lookupTermDebruijn :: Debrujin -> Typechecker (String, Core.Ty)
lookupTermDebruijn idx = do
  env <- ask
  if idx < length (termEnv env)
    then pure $ termEnv env !! idx
    else throwError $ "Unbound term variable: " ++ show idx

-- | Extend the type environment with a new type variable, and run a new
-- computation in that extended environment.
withType :: Core.Ty -> Typechecker a -> Typechecker a
withType ty = local (\env -> env{typeEnv = ty : typeEnv env})

-- | Extend the type variable name environment with a new type variable name, and run a new
-- computation in that extended environment.
withTyVar :: String -> Typechecker a -> Typechecker a
withTyVar name = local (\env -> env{tyVarNameEnv = name : tyVarNameEnv env})

-- | Extend the term environment with a new term variable, and run a new
-- computation in that extended environment.
withTerm :: String -> Core.Ty -> Typechecker a -> Typechecker a
withTerm name ty = local (\env -> env{termEnv = (name, ty) : termEnv env})

-------------------------------------------------------------
-- System F
-------------------------------------------------------------

tyMap :: (Int -> Int -> Core.Ty) -> Int -> Core.Ty -> Core.Ty
tyMap f idx ty = walk ty
 where
  -- Walk through the type and apply the mapping function to type variables
  walk t@(Core.TVar n)
    | n == idx = f idx n
    | otherwise = t
  walk (Core.TFunc t1 t2) = Core.TFunc (walk t1) (walk t2)
  walk (Core.TForall t) = Core.TForall (walk t)
  walk (Core.TLam n t) = Core.TLam n (walk t)
  walk (Core.TApp t1 t2) = Core.TApp (walk t1) (walk t2)
  walk (Core.TCons name tys) = Core.TCons name (map walk tys)
  walk other = other

tyShift :: Int -> Core.Ty -> Core.Ty
tyShift d = tyMap shiftVar d
 where
  shiftVar cut idx
    | idx >= cut = Core.TVar (idx + d)
    | otherwise = Core.TVar idx

tySubst :: Int -> Core.Ty -> Core.Ty -> Core.Ty
tySubst j s = tyMap substVar j
 where
  substVar cut idx
    | idx == cut = tyShift cut s
    | idx > j + cut = Core.TVar (idx - 1)
    | otherwise = Core.TVar idx

---------------------------------------------
-- TYPE CHECKING
---------------------------------------------

assertTy :: Core.Ty -> Core.Ty -> Typechecker ()
assertTy expected actual =
  unless (expected == actual) $
    throwError $
      "Type mismatch: expected " ++ show expected ++ ", but got " ++ show actual

inferLit :: AST.Literal -> Typechecker Core.Ty
inferLit (AST.LitInt _) = return Core.TInt
inferLit (AST.LitFloat _) = return Core.TFloat
inferLit (AST.LitBool _) = return Core.TBool
inferLit (AST.LitString _) = return Core.TString

inferExpr :: Core.Expr -> Typechecker Core.Ty
inferExpr = \case
  Core.ELit lit -> inferLit lit
  Core.EUnit -> pure Core.TUnit
  Core.EVar idx -> do
    (_, ty) <- lookupTermDebruijn idx
    pure ty
  Core.ELam argTy body -> do
    -- Extend the term environment with a new variable for the argument
    bodyTy <- withTerm "_lam" argTy (inferExpr body)
    pure $ Core.TFunc argTy bodyTy
  -- Type abstraction: /\a -> body: introduces a new type variable binder
  Core.EAbs body -> do
    -- Extend the type environment with a new type variable for the abstraction
    bodyTy <- withTyVar "_abs" (inferExpr body)
    pure $ Core.TForall bodyTy
  -- Type application e [T]: substitutes a type argument into a polymorphic type
  Core.ETyApp e tyArg -> do
    eTy <- inferExpr e
    case eTy of
      Core.TForall bodyTy -> do
        -- Substitute the type argument into the body type
        let substitutedTy = tySubst 0 tyArg bodyTy
        pure substitutedTy
      _ -> throwError $ "Expected a forall type, but got: " ++ show eTy
  -- Term application: e1 e2
  Core.EApp e1 e2 -> do
    e1Ty <- inferExpr e1
    case e1Ty of
      -- Function type
      Core.TFunc argTy retTy -> do
        e2Ty <- inferExpr e2
        assertTy argTy e2Ty
        pure retTy
      _ -> throwError $ "Expected a function type, but got: " ++ show e1Ty
  Core.ELet varTy e1 e2 -> do
    -- Infer the type of the first expression
    expr1Ty <- inferExpr e1
    assertTy varTy expr1Ty
    -- Extend the term environment with the new variable and infer the type of the second expression
    withTerm "_let" varTy (inferExpr e2)
  Core.EMatch e branches -> do
    eTy <- inferExpr e

    branchTys <- forM branches $ \(pat, branchExpr) -> do
      -- Check the pattern against the type of the matched expression,
      -- for each branch and extend the term environment with any variables bound by it
      bindings <- checkPat pat eTy
      local (\env -> env{termEnv = bindings ++ termEnv env}) $
        (inferExpr branchExpr)

    case branchTys of
      [] -> throwError "Match expression has no branches"
      (firstTy : restTys) -> do
        mapM_ (assertTy firstTy) restTys
        pure firstTy

checkLit :: AST.Literal -> Core.Ty -> Typechecker Core.Expr
checkLit lit expectedTy = do
  litTy <- inferLit lit
  assertTy expectedTy litTy
  return $ Core.ELit lit

checkExpr :: Core.Expr -> Core.Ty -> Typechecker Core.Expr
checkExpr expr expectedTy = case expr of
  Core.ELit lit -> checkLit lit expectedTy
  Core.EUnit -> do
    assertTy expectedTy Core.TUnit
    pure Core.EUnit
  Core.EVar idx -> do
    (_, ty) <- lookupTermDebruijn idx
    assertTy expectedTy ty
    pure $ Core.EVar idx
  Core.ELam argTy body -> do
    case expectedTy of
      Core.TFunc expectedArgTy expectedRetTy -> do
        assertTy expectedArgTy argTy
        -- Extend the term environment with a new variable for the argument
        lamBody <- withTerm "_lam" argTy (checkExpr body expectedRetTy)
        pure $ Core.ELam argTy lamBody
      _ -> throwError $ "Expected a function type, but got: " ++ show expectedTy
  Core.EAbs body -> do
    case expectedTy of
      Core.TForall expectedBodyTy -> do
        -- Extend the type environment with a new type variable for the abstraction
        absBody <-
          withTyVar "_abs" $
            withType (Core.TVar 0) $
              checkExpr body expectedBodyTy

        pure $ Core.EAbs absBody
      _ -> throwError $ "Expected a forall type, but got: " ++ show expectedTy
  Core.ETyApp e tyArg -> do
    eTy <- inferExpr e
    case eTy of
      Core.TForall bodyTy -> do
        -- Substitute the type argument into the body type
        let substitutedTy = tySubst 0 tyArg bodyTy
        assertTy expectedTy substitutedTy
        checkedE <- checkExpr e eTy
        pure $ Core.ETyApp checkedE tyArg
      _ -> throwError $ "Expected a forall type, but got: " ++ show eTy
  Core.EApp e1 e2 -> do
    e1Ty <- inferExpr e1
    case e1Ty of
      -- Function type
      Core.TFunc argTy retTy -> do
        assertTy expectedTy retTy
        checkedE2 <- checkExpr e2 argTy
        pure $ Core.EApp e1 checkedE2
      _ -> throwError $ "Expected a function type, but got: " ++ show e1Ty
  Core.ELet varTy val body -> do
    -- Infer the type of the first expression
    valTy <- inferExpr val
    assertTy varTy valTy

    -- Extend the term environment with the new variable and check the type of the second expression
    checkedBody <- withTerm "_let" varTy (checkExpr body expectedTy)
    pure $ Core.ELet varTy val checkedBody
  Core.EMatch e branches -> do
    eTy <- inferExpr e

    branchTys <- forM branches $ \(pat, branchExpr) -> do
      -- Check the pattern against the type of the matched expression,
      -- for each branch and extend the term environment with any variables bound by it
      bindings <- checkPat pat eTy
      local (\env -> env{termEnv = bindings ++ termEnv env}) $
        (inferExpr branchExpr)

    case branchTys of
      [] -> throwError "Match expression has no branches"
      (firstTy : restTys) -> do
        forM_ restTys $ \branchTy -> assertTy firstTy branchTy
        assertTy expectedTy firstTy
        pure $ Core.EMatch e branches

-- Subsumption: Fallback case
-- _ -> do
--   inferredTy <- inferExpr expr
--   assertTy expectedTy inferredTy
--   pure expr

checkPat :: AST.Pat -> Core.Ty -> Typechecker [(String, Core.Ty)]
checkPat pat expectedTy = case pat of
  AST.PLit lit -> do
    litTy <- inferLit lit
    assertTy expectedTy litTy
    pure []
  AST.PCapture name -> do
    -- Capture the variable in the pattern and bind it to the expected type
    pure [(name, expectedTy)]
  AST.PWildcard -> pure [] -- Wildcard pattern does not bind any variables
  _ ->
    throwError $
      "Pattern matching for this pattern type is not implemented yet: "
        ++ show pat

-- | Run the typechecker pass over a given type environment and a computation
runTypechecker :: TypecheckEnv -> Typechecker a -> Either String a
runTypechecker env m = runExcept (runReaderT (runTypecheckM m) env)
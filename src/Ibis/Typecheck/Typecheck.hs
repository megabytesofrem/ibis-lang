{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Typecheck.Typecheck where

import Control.Monad.Except (MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader (..), runReaderT)

import Control.Monad (unless)

import Ibis.Syntax.AST.Core (Debrujin)
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Syntax.AST.Surface qualified as AST
import Ibis.Typecheck.Environment (TypecheckEnv (..), TypecheckM (..))

-- | Mapping function for types, used in System F substitution and shifting
type Mapper a = a -> a -> Core.Ty

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

-- | Extend the term environment with a new term variable, and run a new
-- computation in that extended environment.
withTerm :: String -> Core.Ty -> Typechecker a -> Typechecker a
withTerm name ty = local (\env -> env{termEnv = (name, ty) : termEnv env})

-------------------------------------------------------------
-- System F substitution and shifting functions for types
-------------------------------------------------------------

tyMap :: Mapper Int -> Int -> Core.Ty -> Core.Ty
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

lowerTy :: Core.Ty -> Typechecker Core.Ty
lowerTy _ = undefined -- Placeholder for type lowering logic

inferLit :: AST.Literal -> Typechecker Core.Ty
inferLit _ = undefined -- Placeholder for literal type inference logic

inferExpr :: Core.Expr -> Typechecker Core.Ty
inferExpr _ = undefined -- Placeholder for type inference logic

checkLit :: AST.Literal -> Core.Ty -> Typechecker ()
checkLit _ _ = undefined -- Placeholder for literal type checking logic

checkExpr :: Core.Expr -> Core.Ty -> Typechecker ()
checkExpr _ _ = undefined -- Placeholder for expression type checking logic

checkPat :: AST.Pat -> Core.Ty -> Typechecker ()
checkPat _ _ = undefined -- Placeholder for pattern type checking logic

-- | Run the typechecker pass over a given type environment and a computation
runTypechecker :: TypecheckEnv -> Typechecker a -> Either String a
runTypechecker env m = runExcept (runReaderT (runTypecheckM m) env)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
  Module      : Ibis.Typecheck.Check
  Description : Type checking and inference for Ibis' core language
-}
module Ibis.Typecheck.Check where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks, local)

import Ibis.Syntax.AST.Core
import Ibis.Typecheck.Eval (convert, eval, evalFst, isDefEq, readBack)

data TypecheckCtx = TypecheckCtx
  { env :: [Value]
  -- ^ Evaluation environment for term evaluation
  , typingCtx :: [Value]
  -- ^ Typing context Γ
  }

-- | Typecheck monad used for type checking and inference
newtype TypecheckM a = TypecheckM {runTypechecker :: ReaderT TypecheckCtx (Either String) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader TypecheckCtx
    , MonadError String
    )

lookupType :: DeBruijn -> TypecheckM Value
lookupType idx = do
  ctx <- asks typingCtx
  if idx < length ctx
    then pure (ctx !! idx)
    else throwError $ "Unbound type variable: " ++ show idx

lookupValue :: DeBruijn -> TypecheckM Value
lookupValue idx = do
  ctx <- asks env
  if idx < length ctx
    then pure (ctx !! idx)
    else throwError $ "Unbound variable: " ++ show idx

withBinding :: Value -> Value -> TypecheckM a -> TypecheckM a
withBinding v ty =
  local
    ( \ctx ->
        ctx
          { env = v : env ctx
          , typingCtx = ty : typingCtx ctx
          }
    )

---------------------------------------------

-- | Get the universe level of a type, throwing an error if it's not a universe
-- NOTE: Universe 0 is the universe of propositions, Universe 1 is the universe of types, and so on.
universeLevel :: Value -> TypecheckM Int
universeLevel (VUniverse n) = pure n
universeLevel v = throwError $ "Type mismatch: Expected a universe type, but got: " ++ show v

-- | Check a term against an expected type, throwing an error if they do not match
check :: CoreTerm -> Value -> TypecheckM ()
check term ty = case (term, ty) of
  -- Lambda abstraction: checked against a dependent function type (Pi)
  (Lam _lname body, VPi _vname dom cod) -> do
    -- Lookup the current variable level to create a fresh variable for the domain type
    varLevel <- asks (length . env)

    -- Create a fresh variable of the domain type and extend the environment and typing context
    let freshVar = VNeutral dom (NVar varLevel)
        expectedBodyTy = cod freshVar
     in withBinding freshVar dom $ check body expectedBodyTy
  (Lam _ _, expectedTy) -> do
    depth <- asks (length . typingCtx)
    let normExpected = readBack depth expectedTy
    throwError $
      "Type mismatch: Expected a dependent function type (Pi) for lambda abstraction, but got: "
        ++ show normExpected
  -- Pair constructor: checked against a dependent product type (Sigma)
  (Pair a b, VSigma _name dom cod) -> do
    -- Check the first component against the domain type
    check a dom
    env' <- asks env
    let aVal = eval env' a
        expectedBty = cod aVal
     in check b expectedBty
  (Pair _ _, expectedTy) -> do
    depth <- asks (length . typingCtx)
    let normExpected = readBack depth expectedTy
    throwError $
      "Type mismatch: Expected a dependent product type (Sigma) for pair constructor, but got: "
        ++ show normExpected
  --
  -- Fallback case: infer the type of the term and compare it with the expected type
  (expectedTerm, expectedTy) -> do
    inferredTy <- infer expectedTerm
    depth <- asks (length . typingCtx)

    if convert depth inferredTy expectedTy
      then pure ()
      else do
        let normExpected = readBack depth expectedTy
            normInferred = readBack depth inferredTy
        throwError $
          "Type mismatch:\n Expected: "
            ++ show normExpected
            ++ "\n  Inferred: "
            ++ show normInferred

-- | Infer the type of a term, throwing an error if it cannot be inferred
infer :: CoreTerm -> TypecheckM Value
infer term = case term of
  -- Universes: Universe n has type Universe (n + 1)
  Universe n -> pure $ VUniverse (n + 1)
  --
  -- Base types and literals
  Unit -> pure VUnit
  Lit l -> pure $ VLit l
  --

  Var idx -> lookupType idx
  --
  -- Dependent function types (Pi) : Π(x : A). B
  Pi _name dom cod -> do
    -- Check that domain is a valid type (i.e., has type Universe n for some n)
    domTy <- infer dom
    domLevel <- universeLevel domTy

    env' <- asks env
    varLevel <- asks (length . env)

    let domVal = eval env' dom
        freshVar = VNeutral domVal (NVar varLevel)

    codLevel <- withBinding freshVar domVal $ do
      codTy <- infer cod
      universeLevel codTy

    pure $ VUniverse (max domLevel codLevel)

  --
  -- Function application: f a
  App f x -> do
    fTy <- infer f
    case fTy of
      VPi _name dom cod -> do
        -- Check the argument against the domain type
        check x dom
        env' <- asks env
        let xVal = eval env' x
        pure $ cod xVal
      _ -> do
        depth <- asks (length . typingCtx)
        let normFty = readBack depth fTy
        throwError $
          "Type mismatch: Expected a function type (Pi or ->) for application, but got: "
            ++ show normFty
  --

  -- Dependent product types (Sigma) : Σ(x : A). B
  Sigma _name dom cod -> do
    -- Check that domain is a valid type
    domTy <- infer dom
    domLevel <- universeLevel domTy

    env' <- asks env
    varLevel <- asks (length . env)
    let domVal = eval env' dom
        freshVar = VNeutral domVal (NVar varLevel)

    codLevel <- withBinding freshVar domVal $ do
      codTy <- infer cod
      universeLevel codTy

    pure $ VUniverse (max domLevel codLevel)
  --
  Fst p -> do
    pTy <- infer p
    case pTy of
      VSigma _name dom cod -> pure dom
      _ -> do
        depth <- asks (length . typingCtx)
        let normPty = readBack depth pTy
        throwError $
          "Type mismatch: Expected a dependent product type (Sigma) for fst, but got: "
            ++ show normPty
  Snd p -> do
    pTy <- infer p
    case pTy of
      VSigma _name dom cod -> do
        env' <- asks env
        let pVal = eval env' p
        pure $ cod (evalFst pVal)
      _ -> do
        depth <- asks (length . typingCtx)
        let normPty = readBack depth pTy
        throwError $
          "Type mismatch: Expected a dependent product type (Sigma) for snd, but got: "
            ++ show normPty
  Pair _ _ -> do
    env' <- asks env
    depth <- asks (length . typingCtx)
    let normTerm = readBack depth (eval env' term)
    throwError $ "Cannot infer type for pair constructor: " ++ show normTerm
  Lam _ _ -> do
    env' <- asks env
    depth <- asks (length . typingCtx)
    let normTerm = readBack depth (eval env' term)
    throwError $ "Cannot infer type for lambda abstraction: " ++ show normTerm
  _ -> do
    env' <- asks env
    depth <- asks (length . typingCtx)
    let normTerm = readBack depth (eval env' term)
    throwError $ "Cannot infer type for term (" ++ show term ++ "): " ++ show normTerm
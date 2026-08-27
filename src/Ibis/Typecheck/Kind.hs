{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Typecheck.Kind where

import Ibis.Syntax.AST.Core (Debrujin)
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Syntax.AST.Kind (Kind (..))

type KindEnv = [Kind]

data KindCheckEnv = KindCheckEnv
  { kindEnv :: KindEnv
  , kindCtorEnv :: [(String, Kind)] -- Kind environment for type constructors
  }
  deriving (Show, Eq)

mkKindCheckEnv :: KindEnv -> KindCheckEnv
mkKindCheckEnv env = KindCheckEnv{kindEnv = env, kindCtorEnv = defaultCtorKindEnv}
 where
  defaultCtorKindEnv =
    [ ("List", KArrow KStar KStar) -- List : * -> *
    , ("Maybe", KArrow KStar KStar) -- Maybe : * -> *
    , ("Bool", KStar) -- Bool : *
    ]

-- | Lookup the kind of type variable in the kind environment
lookupKind :: KindCheckEnv -> Debrujin -> Either String Kind
lookupKind env idx
  | idx < length (kindEnv env) = Right (kindEnv env !! idx)
  | otherwise = Left ("Unbound type variable: " ++ show idx)

-- | Assert that a type must have a specific kind, returning an error if it does not
assertKind :: KindCheckEnv -> Core.Ty -> Kind -> Either String ()
assertKind env ty expectedK = do
  k <- inferKind env ty
  _ <- unifyKinds k expectedK
  Right ()

-- | Infer the kind of a type, returning an error if it cannot be inferred
inferKind :: KindCheckEnv -> Core.Ty -> Either String Kind
inferKind _ Core.TInt = Right KStar
inferKind _ Core.TFloat = Right KStar
inferKind _ Core.TBool = Right KStar
inferKind _ Core.TString = Right KStar
inferKind _ Core.TUnit = Right KStar
inferKind env (Core.TVar idx _) = lookupKind env idx
inferKind env (Core.TFunc arg ret) = do
  kArg <- inferKind env arg
  kRet <- inferKind env ret
  unifyKinds kArg KStar >> unifyKinds kRet KStar >> Right KStar
inferKind env (Core.TForall k ty) = do
  kTy <- inferKind env{kindEnv = k : kindEnv env} ty
  unifyKinds kTy KStar >> Right KStar
inferKind env (Core.TLam k body) = do
  kBody <- inferKind env{kindEnv = k : kindEnv env} body
  Right (KArrow k kBody)
inferKind env (Core.TApp t1 t2) = do
  k1 <- inferKind env t1
  k2 <- inferKind env t2
  case k1 of
    KArrow kArg kRes -> do
      _ <- unifyKinds kArg k2
      Right kRes
    _ ->
      Left $ "Expected a type constructor (* -> *), but got: " ++ show k1
inferKind env (Core.TCons name params) = case lookup name (kindCtorEnv env) of
  Just ctorKind -> do
    paramKinds <- traverse (inferKind env) params
    let expectedKind = foldr KArrow KStar paramKinds
    _ <- unifyKinds ctorKind expectedKind
    Right KStar

  -- Unknown type constructor
  Nothing -> Left ("Unknown type constructor: " ++ name)

-- | Unify two kinds, returning an error if they cannot be unified
unifyKinds :: Kind -> Kind -> Either String Kind
unifyKinds KStar KStar = Right KStar
unifyKinds (KArrow k1 k2) (KArrow k1' k2') = do
  k1'' <- unifyKinds k1 k1'
  k2'' <- unifyKinds k2 k2'
  Right (KArrow k1'' k2'')
unifyKinds k1 k2 =
  Left $ "Unification error: kind mismatch " ++ show k1 ++ " and " ++ show k2

-- | Check whether a list of types all have kind *
allStars :: KindCheckEnv -> [Core.Ty] -> Either String ()
allStars env tys = mapM_ (\ty -> assertKind env ty KStar) tys
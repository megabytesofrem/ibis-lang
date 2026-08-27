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

inferKind :: KindCheckEnv -> Core.Ty -> Either String Kind
inferKind _ Core.TInt = Right KStar
inferKind _ Core.TFloat = Right KStar
inferKind _ _ = Left "Kind inference not implemented for this type"

unifyKinds :: Kind -> Kind -> Either String Kind
unifyKinds KStar KStar = Right KStar
unifyKinds _ _ = Left "Kind unification not implemented for these kinds"
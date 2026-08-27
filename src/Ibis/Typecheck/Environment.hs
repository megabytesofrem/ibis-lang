{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Typecheck.Environment where

import Control.Monad.Except (Except, MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT)

import Ibis.Syntax.AST.Core qualified as Core

-- | TypecheckM monad with access to a type environment and the ability to throw errors
--
-- Should probably make this Except CustomError instead of String later on; but for now
-- this is sufficient for prototyping.
newtype TypecheckM env a = TypecheckM
  { runTypecheckM :: ReaderT env (Except String) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader env
    , MonadError String
    )

-- | Environment used during typechecking, containing the type environment
-- and term environment
data TypecheckEnv = TypecheckEnv
  { typeEnv :: [Core.Ty]
  , termEnv :: [(String, Core.Ty)]
  , tyVarNameEnv :: [String]
  -- ^ Environment for type variable names, used for pretty-printing
  }
  deriving (Show, Eq)

-- | Create a new empty typechecking environment
mkTypecheckEnv :: TypecheckEnv
mkTypecheckEnv =
  TypecheckEnv
    { typeEnv = []
    , termEnv = defaultTermEnv
    , tyVarNameEnv = []
    }

defaultTermEnv :: [(String, Core.Ty)]
defaultTermEnv =
  [ ("True", Core.TBool)
  , ("False", Core.TBool)
  , ("Nil", Core.TCons "List" [Core.TVar 0]) -- List a
  ,
    ( "Cons"
    , Core.TFunc
        (Core.TVar 0)
        (Core.TFunc (Core.TCons "List" [Core.TVar 0]) (Core.TCons "List" [Core.TVar 0])) -- a -> List a -> List a
    )
  ]

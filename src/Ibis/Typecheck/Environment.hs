module Ibis.Typecheck.Environment where

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE ImportQualifiedPost #-}

-- module Ibis.Typecheck.Environment where

-- import Control.Monad.Except (Except, MonadError (..))
-- import Control.Monad.Reader (MonadReader (..), ReaderT)

-- import Ibis.Syntax.AST.Core qualified as Core
-- import Ibis.Syntax.AST.Kind (Kind (..))
-- import Ibis.Typecheck.Kind (KindCheckEnv, mkKindCheckEnv)

-- -- | TypecheckM monad with access to a type environment and the ability to throw errors
-- --
-- -- Should probably make this Except CustomError instead of String later on; but for now
-- -- this is sufficient for prototyping.
-- newtype TypecheckM env a = TypecheckM
--   { runTypecheckM :: ReaderT env (Except String) a
--   }
--   deriving
--     ( Functor
--     , Applicative
--     , Monad
--     , MonadReader env
--     , MonadError String
--     )

-- -- | Environment used during typechecking, containing the type environment
-- -- and term environment
-- data TypecheckEnv = TypecheckEnv
--   { typeEnv :: [Core.Ty]
--   , termEnv :: [(String, Core.Ty)]
--   , tyVarNameEnv :: [String]
--   -- ^ Environment for type variable names, used for pretty-printing
--   , kindCheckEnv :: KindCheckEnv
--   -- ^ Environment for kind checking
--   }
--   deriving (Show, Eq)

-- -- | Create a new empty typechecking environment
-- mkTypecheckEnv :: TypecheckEnv
-- mkTypecheckEnv =
--   TypecheckEnv
--     { typeEnv = []
--     , termEnv = defaultTermEnv
--     , tyVarNameEnv = []
--     , kindCheckEnv = mkKindCheckEnv []
--     }

-- defaultTermEnv :: [(String, Core.Ty)]
-- defaultTermEnv =
--   [ ("True", Core.TBool)
--   , ("False", Core.TBool)
--   , ("Nil", Core.TCons "List" [Core.TVar 0 KStar]) -- List a
--   ,
--     ( "Cons"
--     , Core.TFunc
--         (Core.TVar 0 KStar)
--         (Core.TFunc (Core.TCons "List" [Core.TVar 0 KStar]) (Core.TCons "List" [Core.TVar 0 KStar])) -- a -> List a -> List a
--     )
--   ]

{-# LANGUAGE MultiParamTypeClasses #-}

module Ibis.Prettyprint
  ( Prettyprint (..)
  , Prettyprinter (unPretty)
  , PrettyprintCtx (..)

    -- * Prettyprint constructors
  , mkPrettyprintCtx
  , mkPrettyprinter

    -- * Prettyprint functions
  , prettyPrint
  , indentToSpaces
  , indentBlock
  , getIndent
  , withIndentCtx
  , evalPrettyPrintStep
  , runPrettyPrint
  )
where

import Control.Monad.State (MonadState (..), State, evalState, gets, modify, runState)

-- | Prettyprint context to keep track of indentation level
newtype PrettyprintCtx = PrettyprintCtx
  { indentLevel :: Int
  }
  deriving (Show, Eq)

-- | Prettyprinter monad for pretty printing with indentation
newtype Prettyprinter a = Prettyprinter
  { unPretty :: State PrettyprintCtx a
  }

class Prettyprint a where
  pretty :: a -> Prettyprinter String

-- INSTANCES

instance Functor Prettyprinter where
  fmap f (Prettyprinter s) = Prettyprinter{unPretty = (fmap f s)}

instance Applicative Prettyprinter where
  pure x = Prettyprinter{unPretty = pure x}
  (Prettyprinter f) <*> (Prettyprinter s) = Prettyprinter{unPretty = f <*> s}

instance Monad Prettyprinter where
  (Prettyprinter s) >>= f = Prettyprinter{unPretty = s >>= unPretty . f}

instance MonadState PrettyprintCtx Prettyprinter where
  get = Prettyprinter{unPretty = get}
  put s = Prettyprinter{unPretty = put s}

-- PRETTY PRINT FUNCTIONS

mkPrettyprintCtx :: Int -> PrettyprintCtx
mkPrettyprintCtx indent = PrettyprintCtx{indentLevel = indent}

mkPrettyprinter :: State PrettyprintCtx a -> Prettyprinter a
mkPrettyprinter s = Prettyprinter{unPretty = s}

prettyPrint :: (Prettyprint a, Traversable t) => t a -> Prettyprinter (t String)
prettyPrint = traverse pretty

indentToSpaces :: PrettyprintCtx -> String
indentToSpaces p = replicate (indentLevel p * 2) ' '

indentBlock :: String -> Prettyprinter String
indentBlock block = do
  indent <- getIndent
  let ls = lines block
  pure $ unlines (map (\line -> indent ++ line) ls)

getIndent :: Prettyprinter String
getIndent = gets indentToSpaces

withIndentCtx :: Prettyprinter String -> Prettyprinter String
withIndentCtx action = do
  oldIndent <- gets indentLevel
  modify $ \s -> s{indentLevel = oldIndent + 1}
  result <- action
  modify $ \s -> s{indentLevel = oldIndent}
  pure result

-- | Evaluate a single pretty print step and return the resulting string.
evalPrettyPrintStep :: (Prettyprint a) => a -> String
evalPrettyPrintStep x = evalState (unPretty (pretty x)) (PrettyprintCtx{indentLevel = 0})

-- | Run the pretty printer and return the resulting string along with the final context.
runPrettyPrint :: (Prettyprint a) => a -> PrettyprintCtx -> (String, PrettyprintCtx)
runPrettyPrint x s = runState (unPretty (pretty x)) s
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ibis.Tactics.Prover where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState, StateT, get, modify, put, runStateT)

import qualified Data.Map.Strict as M
import Ibis.Syntax.AST.Surface (Term (..))

type GoalId = Int

-- | A goal in the proof state
data Goal
  = Goal
  { goalId :: !GoalId
  , goalHyps :: M.Map String Term -- Hypotheses in the context of the goal
  , goalTarget :: !Term
  }
  deriving (Show, Eq)

data ProverCtx = ProverCtx
  { activeGoals :: ![Goal]
  , solvedGoals :: ![Goal]
  , nextGoalId :: !GoalId
  }
  deriving (Show, Eq)

-- | Prover monad used for proof search and goal management
newtype ProverM a = ProverM {runProverM :: StateT ProverCtx (Either String) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState ProverCtx
    , MonadError String
    )

mkProverCtx :: Term -> ProverCtx
mkProverCtx target =
  ProverCtx
    { activeGoals = [Goal 0 M.empty target]
    , solvedGoals = []
    , nextGoalId = 1
    }

runProver :: ProverM a -> ProverCtx -> Either String (a, ProverCtx)
runProver prover ctx = runStateT (runProverM prover) ctx

---------------------------------------------

freshGoalId :: ProverM GoalId
freshGoalId = do
  ctx <- get
  let gid = nextGoalId ctx
  put $ ctx{nextGoalId = gid + 1}
  pure gid

currentGoal :: ProverM Goal
currentGoal = do
  ctx <- get
  case activeGoals ctx of
    [] -> throwError "No active goals"
    (g : _) -> pure g

popCurrentGoal :: ProverM Goal
popCurrentGoal = do
  ctx <- get
  case activeGoals ctx of
    [] -> throwError "No active goals to pop"
    (g : gs) -> do
      put $ ctx{activeGoals = gs}
      pure g

pushGoal :: Goal -> ProverM ()
pushGoal goal = pushGoals [goal]

pushGoals :: [Goal] -> ProverM ()
pushGoals newGoals = modify $ \s -> s{activeGoals = newGoals ++ activeGoals s}

addHypothesis :: String -> Term -> ProverM ()
addHypothesis name typ = do
  goal <- popCurrentGoal
  let updatedHyps = M.insert name typ (goalHyps goal)
  pushGoal $ goal{goalHyps = updatedHyps}

---------------------------------------------

-- Term substitution function
subst :: String -> Term -> Term -> Term
subst x sub target = case target of
  Var v
    | v == x -> sub
    | otherwise -> Var v
  App f a -> App (subst x sub f) (subst x sub a)
  Lam v body
    | v == x -> Lam v body -- shadowed: do not substitute in body
    | otherwise -> Lam v (subst x sub body)
  other -> other
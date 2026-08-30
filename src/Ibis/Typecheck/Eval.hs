{- |
  Module      : Ibis.Typecheck.Eval
  Description : Evaluation of dependent terms in the core language and read-backs
-}
module Ibis.Typecheck.Eval where

import Control.Monad (zipWithM)
import Ibis.Syntax.AST (Pat (..))
import Ibis.Syntax.AST.Core

type Env = [Value]

-- Definitional equality check for two values in a given environment
isDefEq :: Env -> CoreTerm -> CoreTerm -> Bool
isDefEq env t1 t2 =
  let v1 = eval env t1
      v2 = eval env t2
   in convert (length env) v1 v2

-- Equality check for two values in a given environment
equal :: Env -> Value -> Value -> Bool
equal env v1 v2 = convert (length env) v1 v2

-- Convert two values to their normal forms and check for equality using
-- their read-back representations
convert :: DeBruijn -> Value -> Value -> Bool
convert depth v1 v2 =
  readBack depth v1 == readBack depth v2

-- | Safe lookup in the environment, handles out-of-bounds indices gracefully
lookupEnv :: Int -> Env -> Maybe Value
lookupEnv idx env
  | idx < length env = Just (env !! idx)
  | otherwise = Nothing

extendEnv :: Value -> Env -> Env
extendEnv v env = v : env

-- Evaluate a CoreTerm in a given environment to produce a Value
eval :: Env -> CoreTerm -> Value
eval env term = case term of
  Universe n -> VUniverse n
  Var n -> case lookupEnv n env of
    Just v -> v
    Nothing -> error $ "Unbound variable at index: " ++ show n
  Lit l -> VLit l
  Unit -> VUnit
  -- Dependent Functions
  Pi name dom cod ->
    VPi name (eval env dom) (\x -> eval (x : env) cod)
  Lam name body ->
    let dummyDom = VUnit
     in VLam name dummyDom (\x -> eval (x : env) body)
  App f x ->
    evalApp (eval env f) (eval env x)
  -- Dependent Products
  Sigma name dom cod ->
    VSigma name (eval env dom) (\x -> eval (x : env) cod)
  Pair a b ->
    VPair (eval env a) (eval env b)
  Fst p -> evalFst (eval env p)
  Snd p -> evalSnd (eval env p)
  -- Let binding
  Let _name e body ->
    let v = eval env e
     in eval (extendEnv v env) body
  -- Pattern Matching
  Match e branches -> evalMatch env (eval env e) branches
  -- Topological Presheaf Primitives
  Site name -> VSite name
  Cover u v -> VCover (eval env u) (eval env v)
  Sect a u -> VSect (eval env a) (eval env u)
  Res a u v proof s -> evalRes (eval env a) (eval env u) (eval env v) (eval env proof) (eval env s)
  Ext a u v proof s -> evalExt (eval env a) (eval env u) (eval env v) (eval env proof) (eval env s)

countPatternVars :: Pat -> Int
countPatternVars (PLit _) = 0
countPatternVars (PCapture _) = 1
countPatternVars (PTuple pats) = sum (map countPatternVars pats)
countPatternVars (PCtor _ pats) = sum (map countPatternVars pats)
countPatternVars (PPartition _ pat) = 1 + countPatternVars pat
countPatternVars (PWildcard) = 0

flattenPair :: Value -> [Value]
flattenPair (VPair a b) = a : flattenPair b
flattenPair v = [v]

matchPattern :: Pat -> Value -> Maybe [Value]
matchPattern pat val = case (pat, val) of
  (PLit l1, VLit l2) | l1 == l2 -> Just []
  (PCapture _, v) -> Just [v]
  (PTuple pats, _) -> do
    let vals = flattenPair val
    if length pats == length vals
      then do
        -- Process from RIGHT to LEFT so newest variables are prepended first
        bindingsList <- zipWithM matchPattern (reverse pats) (reverse vals)
        pure $ concat bindingsList
      else Nothing
  (PCtor c1 pats, VCtor c2 vals) | c1 == c2 -> do
    if length pats == length vals
      then do
        -- Process from RIGHT to LEFT so newest variables are prepended first
        bindingsList <- zipWithM matchPattern (reverse pats) (reverse vals)
        pure $ concat bindingsList
      else Nothing
  (PPartition _ tailPat, VCons hd tl) -> do
    tailBindings <- matchPattern tailPat tl
    -- tailBindings are newer than `hd`, so they must sit in front of `hd`
    -- so that tail variables get smaller De Bruijn indices (Var 0, Var 1...)
    pure (tailBindings ++ [hd])
  _ -> Nothing

---------------------------------------------
-- EVALUATION OF TERMS
---------------------------------------------

evalMatch :: Env -> Value -> [(Pat, CoreTerm)] -> Value
evalMatch env val branches = case val of
  -- If the value is a stuck neutral term, the match is stuck!
  VNeutral vType neu ->
    let
      -- Evaluate branch bodies with neutral values for type checking/normalization
      evalBranch (pat, body) =
        let
          -- Create dummy neutral values for pattern variables
          currentDepth = length env
          numVars = countPatternVars pat
          dummyNeuVars = [VNeutral vType (NVar (currentDepth + i)) | i <- [0 .. numVars - 1]]
          bEnv = dummyNeuVars ++ env
         in
          (pat, eval bEnv body)
     in
      VNeutral vType (NMatch neu (map evalBranch branches))
  -- Concrete value: try to match against each pattern in order
  concrete ->
    case findMap (tryBranch concrete) branches of
      Just (bindings, body) ->
        let newEnv = bindings ++ env
         in eval newEnv body
      -- Non-exhaustive pattern match: no branch matched the value
      Nothing -> error "Non-exhaustive pattern match during evaluation"
 where
  tryBranch :: Value -> (Pat, CoreTerm) -> Maybe ([Value], CoreTerm)
  tryBranch v (pat, body) = do
    bindings <- matchPattern pat v
    pure (bindings, body)

  findMap :: (Foldable t) => (a -> Maybe b) -> t a -> Maybe b
  findMap f = foldr (\x acc -> case f x of Just y -> Just y; Nothing -> acc) Nothing

evalFst :: Value -> Value
evalFst (VPair a _) = a
evalFst (VNeutral (VSigma _ aT _dT) neu) = VNeutral aT (NFst neu)
evalFst _ = error "Cannot take fst of non-pair value"

evalSnd :: Value -> Value
evalSnd (VPair _ b) = b
evalSnd (VNeutral (VSigma _ _aT dT) neu) =
  let fstNeu = VNeutral _aT (NFst neu)
      sndType = dT fstNeu
   in VNeutral sndType (NSnd neu)
evalSnd _ = error "Cannot take snd of non-pair value"

evalApp :: Value -> Value -> Value
evalApp (VLam _ dom f) x = f x
evalApp (VNeutral (VPi _ _ cod) neu) x = VNeutral (cod x) (NApp neu x)
evalApp _ _ = error "Cannot apply non-function value"

evalRes :: Value -> Value -> Value -> Value -> Value -> Value
evalRes a u v proof (VNeutral _ neu) = VNeutral (VSect a u) (NRes a u v proof neu)
evalRes _ _ _ _ concreteSection = concreteSection -- Compile-time zero-cost identity pass!

evalExt :: Value -> Value -> Value -> Value -> Value -> Value
evalExt a u v proof (VNeutral _ neu) = VNeutral (VSect a v) (NExt a u v proof neu)
evalExt _ _ _ _ concreteSection = concreteSection

---------------------------------------------
-- READ BACK
---------------------------------------------

--  Convert the Value back into a CoreTerm
readBack :: DeBruijn -> Value -> CoreTerm
readBack depth val = case val of
  VUniverse n -> Universe n
  VLit l -> Lit l
  VUnit -> Unit
  VPi name dom cod ->
    let fresh = VNeutral dom (NVar depth)
        cod' = cod fresh
     in Pi name (readBack depth dom) (readBack (depth + 1) cod')
  VLam name dom body ->
    let fresh = VNeutral dom (NVar depth)
        body' = body fresh
     in Lam name (readBack (depth + 1) body')
  VSigma name dom cod ->
    let fresh = VNeutral dom (NVar depth)
        cod' = cod fresh
     in Sigma name (readBack depth dom) (readBack (depth + 1) cod')
  VPair a b -> Pair (readBack depth a) (readBack depth b)
  VSite name -> Site name
  VCover u v -> Cover (readBack depth u) (readBack depth v)
  VSect a u -> Sect (readBack depth a) (readBack depth u)
  VNeutral ty neu -> readBackNeutral depth ty neu
  _ -> error "Read back for this value is not implemented yet"

-- Convert a neutral term back into a CoreTerm, given its type and the current De Bruijn depth
readBackNeutral :: DeBruijn -> Value -> Neutral -> CoreTerm
readBackNeutral depth ty neu = case neu of
  NVar idx -> Var (depth - idx - 1)
  NApp n x -> App (readBackNeutral depth ty n) (readBack depth x)
  NFst n -> Fst (readBackNeutral depth ty n)
  NSnd n -> Snd (readBackNeutral depth ty n)
  NMatch n branches ->
    let readBackBranch (pat, body) = (pat, readBack (depth + countPatternVars pat) body)
     in Match (readBackNeutral depth ty n) (map readBackBranch branches)
  -- Dummy cases for topological presheaf primitives
  NRes a u v proof neu ->
    Res
      (readBack depth a)
      (readBack depth u)
      (readBack depth v)
      (readBack depth proof)
      (readBackNeutral depth (VSect a v) neu)
  NExt a u v proof neu ->
    Ext
      (readBack depth a)
      (readBack depth u)
      (readBack depth v)
      (readBack depth proof)
      (readBackNeutral depth (VSect a u) neu)

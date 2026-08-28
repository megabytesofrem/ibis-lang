module Ibis.Typecheck.Eval where

import Ibis.Syntax.AST.Core

type Env = [Value]

-- | Check if two terms are alpha-equivalent
alphaEq :: CoreTerm -> CoreTerm -> Bool
alphaEq t1 t2 = error "Alpha-equivalence checking is not implemented yet"

-- | Safe lookup in the environment, handles out-of-bounds indices gracefully
lookupEnv :: Int -> Env -> Maybe Value
lookupEnv idx env
  | idx < length env = Just (env !! idx)
  | otherwise = Nothing

extendEnv :: Value -> Env -> Env
extendEnv v env = v : env

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
    VLam name (\x -> eval (x : env) body)
  App f x ->
    evalApp (eval env f) (eval env x)
  -- Dependent Products
  Sigma name dom cod ->
    VSigma name (eval env dom) (\x -> eval (x : env) cod)
  Pair a b ->
    VPair (eval env a) (eval env b)
  Fst p -> evalFst (eval env p)
  Snd p -> evalSnd (eval env p)
  -- Pattern Matching
  Match e branches -> error "Evaluation for match expressions is not implemented yet"

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
evalApp (VLam _ f) x = f x
evalApp (VNeutral (VPi _ _ cod) neu) x = VNeutral (cod x) (NApp neu x)
evalApp _ _ = error "Cannot apply non-function value"

readBack :: DeBruijn -> Value -> CoreTerm
readBack depth val = case val of
  VUniverse n -> Universe n
  VLit l -> Lit l
  VUnit -> Unit
  VPi name dom cod ->
    let fresh = VNeutral dom (NVar depth)
        cod' = cod fresh
     in Pi name (readBack depth dom) (readBack (depth + 1) cod')
  VLam name body ->
    let fresh = VNeutral (VUniverse 0) (NVar depth)
        body' = body fresh
     in Lam name (readBack (depth + 1) body')
  VSigma name dom cod ->
    let fresh = VNeutral dom (NVar depth)
        cod' = cod fresh
     in Sigma name (readBack depth dom) (readBack (depth + 1) cod')
  VPair a b -> Pair (readBack depth a) (readBack depth b)
  VNeutral ty neu -> readBackNeutral depth ty neu

readBackNeutral :: DeBruijn -> Value -> Neutral -> CoreTerm
readBackNeutral depth ty neu = case neu of
  NVar idx -> Var (depth - idx - 1)
  NApp n x -> App (readBackNeutral depth ty n) (readBack depth x)
  NFst n -> Fst (readBackNeutral depth ty n)
  NSnd n -> Snd (readBackNeutral depth ty n)
  NMatch _n _ -> error "Cannot read back match expressions, not yet implemented"
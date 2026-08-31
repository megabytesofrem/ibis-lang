module Ibis.Syntax.AST.Operator
  ( Unop (..)
  , Binop (..)
  )
where

data Unop
  = OpNegate -- -x
  | OpNot -- not x
  deriving (Eq)

{- FOURMOLU_DISABLE -} 
data Binop
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | -- Heyting operators
    OpAnd   -- x and y
  | OpOr    -- x or y
  | OpEq    -- x == y
  | OpIso   -- x ~= y
  | OpNeq   -- x != y
  | OpLt    -- x < y
  | OpGt    -- x > y
  | OpLeq   -- x <= y
  | OpGeq   -- x >= y
  | OpImply -- x ==> y

  | -- Categorical operators (hardcoded for now)
    OpMap   -- f <$> xs
  | OpApp   -- f <*> xs
  | OpBind  -- xs >>= f
  | OpCompose -- f . g
  deriving (Eq)
{- FOURMOLU_ENABLE -}

instance Show Unop where
  show OpNegate = "-"
  show OpNot = "not"

instance Show Binop where
  show OpAdd = "+"
  show OpSub = "-"
  show OpMul = "*"
  show OpDiv = "/"
  show OpAnd = "and"
  show OpOr = "or"
  show OpEq = "=="
  show OpIso = "~="
  show OpNeq = "!="
  show OpLt = "<"
  show OpGt = ">"
  show OpLeq = "<="
  show OpGeq = ">="
  show OpImply = "==>"
  show OpMap = "<$>"
  show OpApp = "<*>"
  show OpBind = ">>="
  show OpCompose = "."
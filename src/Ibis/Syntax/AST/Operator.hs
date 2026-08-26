module Ibis.Syntax.AST.Operator
  ( Unop (..)
  , Binop (..)
  )
where

data Unop = Negate | Not
  deriving (Eq)

data Binop
  = Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  deriving (Eq)

instance Show Unop where
  show Negate = "-"
  show Not = "not"

instance Show Binop where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show And = "and"
  show Or = "or"
  show Eq = "=="
  show Neq = "!="
  show Lt = "<"
  show Gt = ">"
  show Leq = "<="
  show Geq = ">="
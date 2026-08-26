module Ibis.Syntax.AST.Operator
  ( Unop (..)
  , Binop (..)
  )
where

data Unop = Negate | Not
  deriving (Show, Eq)

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
  deriving (Show, Eq)
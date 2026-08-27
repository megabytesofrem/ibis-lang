module Ibis.Syntax.AST.Kind (Kind (..)) where

data Kind
  = KStar -- The kind of all types
  | KArrow Kind Kind -- The kind of type constructors (* -> *)
  deriving (Show, Eq)
-- | Error types for the typechecker
module Ibis.Typecheck.Error
  ( TcError (..)
  )
where

data TcError
  = UnboundVariable String
  | UnboundTypeVariable String
  | TypeMismatch String
  | CannotInferType String
  | ArityMismatch String Int Int
  | FieldNotFound String String
  | MetavarSolved Int
  | MetavarNotFound Int
  | EmptyBindOutsideDo
  | EmptyDoBlock
  | Other String
  deriving (Eq)

instance Show TcError where
  show (UnboundVariable name) = "Unbound variable: " ++ name
  show (UnboundTypeVariable name) = "Unbound type variable: " ++ name
  show (TypeMismatch msg) = "Type mismatch: " ++ msg
  show (CannotInferType msg) = "Cannot infer type: " ++ msg
  show (ArityMismatch name expected actual) =
    "Arity mismatch for function '" ++ name ++ "': expected " ++ show expected ++ ", got " ++ show actual
  show (FieldNotFound sname fname) =
    "Field '" ++ fname ++ "' not found in structure '" ++ sname ++ "'"
  show (MetavarSolved mvarId) = "Metavariable ?m" ++ show mvarId ++ " has already been solved."
  show (MetavarNotFound mvarId) = "Metavariable ?m" ++ show mvarId ++ " not found."
  show EmptyBindOutsideDo = "Bind statement found outside of a do block."
  show EmptyDoBlock = "Empty do block found."
  show (Other msg) = msg
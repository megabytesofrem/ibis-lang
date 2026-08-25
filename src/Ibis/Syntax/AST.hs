module Ibis.Syntax.AST
  ( Literal (..)
  , Binder (..)
  , Unop (..)
  , Binop (..)
  , Expr (..)
  , Ty (..)
  )
where

data Literal
  = LitInt Integer
  | LitFloat Double
  | LitBool Bool
  | LitString String
  deriving (Show, Eq)

-- Let-bound binders can have optional type annotations
data Binder = Binder String (Maybe Ty)
  deriving (Show, Eq)

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

data Expr
  = ELit Literal
  | EUnit
  | EVar String
  | EUnop Unop Expr
  | EBinop Binop Expr Expr
  | EList [Expr] -- [1, 2, 3]
  | ETuple [Expr] -- (1, 2, 3)
  | EApp Expr Expr -- e₁ e₂
  | ELet Binder Expr Expr -- let x = e₁ in e₂
  | EIf Expr Expr Expr -- if cond then e₁ else e₂
  deriving (Show, Eq)

data Ty
  = TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TFunc Ty Ty -- Function type, e.g. Int -> Int
  | TVar String -- Type variable, e.g. a
  | TForall String Ty -- forall a. a -> a
  | TApp Ty Ty -- Type application, e.g. Maybe Int
  | TCons String [Ty] -- Type constructor with parameters
  deriving (Show, Eq)
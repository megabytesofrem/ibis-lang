module Ibis.Syntax.AST.Surface
  ( Literal (..)
  , Binder (..)
  , Expr (..)
  , Decl (..)
  , Ty (..)
  , Pat (..)

    -- * Declarations
  , FunctionDeclaration (..)
  , DataTypeConstructor (..)
  , DataTypeConstructors
  )
where

import Ibis.Syntax.AST.Operator (Binop, Unop)

-------------------------------------------------------------
-- EXPRESSION NODES
-------------------------------------------------------------

data Literal
  = LitInt Integer
  | LitFloat Double
  | LitBool Bool
  | LitString String
  deriving (Show, Eq)

-- Let-bound binders can have optional type annotations
data Binder = Binder String (Maybe Ty)
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
  | EFor Binder Expr Expr -- for x in e₁: e₂
  | EMatch Expr [(Pat, Expr)] -- match e with | pat -> e
  deriving (Show, Eq)

---------------------------------------------
-- DECLARATION NODES
---------------------------------------------

-- List of data type constructors
type DataTypeConstructors = [DataTypeConstructor]

data DataTypeConstructor
  = RecordConstructor String [(String, Ty)]
  | ProductConstructor String [Ty] -- Ctor Ty1 Ty2
  | TupleConstructor [Ty] -- (Ty1, Ty2)
  deriving (Show, Eq)

data FunctionDeclaration = FunctionDeclaration
  { funcName :: String
  , funcParams :: [Binder]
  , funcReturnType :: Maybe Ty
  , funcBody :: Expr
  }
  deriving (Show, Eq)

data Decl
  = ExprDecl Expr
  | FunctionDecl FunctionDeclaration
  | DataDecl String [String] DataTypeConstructors
  | ImportDecl String (Maybe String) -- import ModuleName [as Alias]
  | ImportDeclExposing String [String] -- import ModuleName exposing (name1, name2)
  deriving (Show, Eq)

---------------------------------------------
-- TYPE AND PATTERN NODES
---------------------------------------------

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

data Pat
  = PLit Literal
  | PCapture String -- x
  | PWildcard -- _
  | PTuple [Pat] -- (p1, p2, p3)
  | PList [Pat] -- [p1, p2, p3]
  | PCtor String [Pat] -- Ctor x y
  | PPartition String Pat -- (x:xs)
  deriving (Show, Eq)
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

import Data.List (intersperse)
import Ibis.Prettyprint
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

newtype Program = Program [Decl]
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

---------------------------------------------
-- PRETTYPRINT INSTANCES
---------------------------------------------

instance Prettyprint Literal where
  pretty (LitInt n) = pure $ show n
  pretty (LitFloat f) = pure $ show f
  pretty (LitBool b) = pure $ if b then "true" else "false"
  pretty (LitString s) = pure $ show s

instance Prettyprint Binder where
  pretty (Binder name mty) = do
    tyStr <- case mty of
      Just ty -> do
        tyStr' <- pretty ty
        pure $ ": " <> tyStr'
      Nothing -> pure ""
    pure $ name <> tyStr

instance Prettyprint Expr where
  pretty (ELit lit) = pretty lit
  pretty EUnit = pure "()"
  pretty (EVar name) = pure name
  pretty (EUnop op expr) = do
    exprStr <- pretty expr
    pure $ show op <> " " <> exprStr
  pretty (EBinop op e1 e2) = do
    e1Str <- pretty e1
    e2Str <- pretty e2
    pure $ "(" <> e1Str <> " " <> show op <> " " <> e2Str <> ")"
  pretty (EList exprs) = do
    exprsStrs <- mapM pretty exprs
    pure $ "[" <> unwords (intersperse ", " exprsStrs) <> "]"
  pretty (ETuple exprs) = do
    exprsStrs <- mapM pretty exprs
    pure $ "(" <> unwords (intersperse ", " exprsStrs) <> ")"
  pretty (EApp e1 e2) = do
    e1Str <- pretty e1
    e2Str <- pretty e2
    pure $ "(" <> e1Str <> " " <> e2Str <> ")"
  pretty (ELet binder e1 e2) = do
    binderStr <- pretty binder
    e1Str <- pretty e1
    e2Str <- withIndentCtx $ pretty e2 >>= indentBlock

    pure $ "let " <> binderStr <> " = " <> e1Str <> " in \n" <> e2Str
  pretty (EIf cond thenExpr elseExpr) = do
    condStr <- pretty cond
    thenStr <- withIndentCtx $ pretty thenExpr >>= indentBlock
    elseStr <- withIndentCtx $ pretty elseExpr >>= indentBlock

    pure $ "if " <> condStr <> " then\n" <> thenStr <> "else\n" <> elseStr
  pretty (EFor binder collection body) = do
    binderStr <- pretty binder
    collectionStr <- pretty collection
    bodyStr <- withIndentCtx $ pretty body >>= indentBlock

    pure $ "for " <> binderStr <> " in " <> collectionStr <> ": " <> bodyStr
  pretty (EMatch expr branches) = do
    exprStr <- pretty expr
    armsStr <- withIndentCtx $ do
      armLines <-
        mapM
          ( \(pat, armExpr) -> do
              patStr <- pretty pat
              armExprStr <- pretty armExpr
              pure $ "| " <> patStr <> " -> " <> armExprStr
          )
          branches

      indentBlock (unlines armLines)
    pure $ "match " <> exprStr <> ":\n" <> armsStr

instance Prettyprint Ty where
  pretty TInt = pure "Int"
  pretty TFloat = pure "Float"
  pretty TBool = pure "Bool"
  pretty TString = pure "String"
  pretty TUnit = pure "Unit"
  pretty (TFunc t1 t2) = do
    t1Str <- pretty t1
    t2Str <- pretty t2
    pure $ "(" <> t1Str <> " -> " <> t2Str <> ")"
  pretty (TVar name) = pure name
  pretty (TForall var ty) = do
    tyStr <- pretty ty
    pure $ "(forall " <> var <> ". " <> tyStr <> ")"
  pretty (TApp t1 t2) = do
    t1Str <- pretty t1
    t2Str <- pretty t2
    pure $ "(" <> t1Str <> " " <> t2Str <> ")"
  pretty (TCons name tys) = do
    tysStrs <- mapM pretty tys
    let tysStr = unwords tysStrs
    pure $ name <> (if null tys then "" else " " <> tysStr)

instance Prettyprint Pat where
  pretty (PLit lit) = pretty lit
  pretty (PCapture name) = pure name
  pretty PWildcard = pure "_"
  pretty (PTuple pats) = do
    patsStrs <- mapM pretty pats
    pure $ "(" <> unwords (intersperse ", " patsStrs) <> ")"
  pretty (PList pats) = do
    patsStrs <- mapM pretty pats
    pure $ "[" <> unwords (intersperse ", " patsStrs) <> "]"
  pretty (PCtor name pats) = do
    patsStrs <- mapM pretty pats
    pure $ name <> if null pats then "" else " " <> unwords patsStrs
  pretty (PPartition first rest) = do
    restStr <- pretty rest
    pure $ first <> " :: " <> restStr

instance Prettyprint DataTypeConstructor where
  pretty (RecordConstructor name fields) = do
    fieldsStrs <-
      mapM
        ( \(fname, fty) -> do
            ftyStr <- pretty fty
            pure $ fname <> " : " <> ftyStr
        )
        fields

    let fieldsStr = unwords (intersperse ", " fieldsStrs)
    pure $ name <> " { " <> fieldsStr <> " }"
  pretty (ProductConstructor name tys) = do
    tysStrs <- mapM pretty tys
    let tysStr = unwords tysStrs
    pure $ name <> " " <> tysStr
  pretty (TupleConstructor tys) = do
    tysStrs <- mapM pretty tys
    let tysStr = unwords (intersperse ", " tysStrs)
    pure $ "(" <> tysStr <> ")"

instance Prettyprint FunctionDeclaration where
  pretty (FunctionDeclaration name params mty body) = do
    paramsStrs <- mapM pretty params
    let paramsStr = unwords paramsStrs
    tyStr <- case mty of
      Just ty -> do
        tyStr' <- pretty ty
        pure $ ": " <> tyStr'
      Nothing -> pure ""
    bodyStr <- withIndentCtx $ pretty body >>= indentBlock

    pure $ name <> " " <> paramsStr <> tyStr <> " =\n" <> bodyStr

instance Prettyprint Decl where
  pretty (ExprDecl expr) = do
    exprStr <- pretty expr
    pure $ exprStr <> ";"
  pretty (FunctionDecl funcDecl) = do
    funcDeclStr <- pretty funcDecl
    pure $ funcDeclStr <> ";"
  pretty (DataDecl name typeVars ctors) = do
    ctorsStrs <- mapM pretty ctors
    let ctorsStr = unwords (intersperse " | " ctorsStrs)
    let typeVarsStr = unwords typeVars
    pure $ "data " <> name <> (if null typeVars then "" else " " <> typeVarsStr) <> " = " <> ctorsStr
  pretty (ImportDecl moduleName malias) = do
    let aliasStr = case malias of
          Just a -> " as " <> a
          Nothing -> ""
    pure $ "import " <> moduleName <> aliasStr <> ";"
  pretty (ImportDeclExposing moduleName names) = do
    let namesStr = unwords (intersperse ", " names)
    pure $ "import " <> moduleName <> " exposing (" <> namesStr <> ");"

instance Prettyprint Program where
  pretty (Program decls) = do
    declsStrs <- mapM pretty decls
    pure $ unlines declsStrs
-- | Surface AST for the Ibis programming language.
module Ibis.Syntax.AST.Surface
  ( Literal (..)
  , Binder (..)
  , Expr (..)
  , Decl (..)
  , Program (..)
  , Ty (..)
  , Pat (..)

    -- * Declarations
  , FunctionParam (..)
  , EnumConstructor (..)
  , EnumDeclaration (..)
  , StructDeclaration (..)
  , FunctionDeclaration (..)
  , ClassDeclaration (..)
  , ClassInstance (..)
  , SiteDeclaration (..)
  , SitePath (..)
  )
where

import Data.List (intersperse)
import Ibis.Prettyprint
import Ibis.Syntax.AST.Kind (Kind)
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
  | ELam [String] Expr -- \x y -> expr
  | ETyAbs [String] Expr -- /\a b -> expr (type abstraction)
  | ETyApp Expr [Ty] -- e [Int, Bool]     (type application)
  | EApp Expr Expr -- e₁ e₂
  | ELet Binder Expr Expr -- let x = e₁ in e₂
  | EIf Expr Expr Expr -- if cond then e₁ else e₂
  | EFor Binder Expr Expr -- for x in e₁: e₂
  | EMatch Expr [(Pat, Expr)] -- match e with | pat -> e
  deriving (Show, Eq)

---------------------------------------------
-- DECLARATION NODES
---------------------------------------------

-- A path in a topological site
data SitePath = SitePath String String String
  deriving (Show, Eq)

-- Function parameters can be binders or type parameters (polymorphism)
data FunctionParam
  = BinderParam Binder
  | TypeParam String Kind -- e.g., a type variable 'α'
  deriving (Show, Eq)

-- Enum constructor with fields, e.g., Some Int, Error String
data EnumConstructor = EnumConstructor
  { enumCtorName :: String
  , enumCtorFields :: [Ty]
  }
  deriving (Show, Eq)

data EnumDeclaration = EnumDeclaration
  { enumName :: String
  , enumConstructors :: [EnumConstructor]
  }
  deriving (Show, Eq)

data StructDeclaration = StructDeclaration
  { structName :: String
  , structFields :: [(String, Ty)]
  }
  deriving (Show, Eq)

data FunctionDeclaration = FunctionDeclaration
  { funcName :: String
  , funcParams :: [FunctionParam]
  , funcReturnType :: Maybe Ty
  , funcBody :: Expr
  }
  deriving (Show, Eq)

data ClassDeclaration = ClassDeclaration
  { className :: String
  , classTypeParams :: [(String, Kind)]
  , classMethods :: [(String, Ty)]
  }
  deriving (Show, Eq)

data ClassInstance = ClassInstance
  { instanceClassName :: String
  , instanceTypeArgs :: [Ty]
  , instanceMethods :: [FunctionDeclaration]
  }
  deriving (Show, Eq)

data SiteDeclaration = SiteDeclaration
  { siteName :: String
  , siteCovers :: [String]
  , sitePaths :: [SitePath]
  }
  deriving (Show, Eq)

data Decl
  = ExprDecl Expr
  | EnumDecl EnumDeclaration
  | StructDecl StructDeclaration
  | FunctionDecl FunctionDeclaration
  | ClassDecl ClassDeclaration
  | ClassInstanceDecl ClassInstance
  | SiteDecl SiteDeclaration
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
  | TVar String (Maybe Kind) -- Type variable, e.g. a or f : * -> *
  | TForall String Kind Ty -- forall (a : k). T
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
  pretty (TVar name Nothing) = pure name
  pretty (TVar name (Just kind)) = pure $ name <> " : " <> show kind
  pretty (TForall var kind ty) = do
    tyStr <- pretty ty
    pure $ "(forall (" <> var <> " : " <> show kind <> "). " <> tyStr <> ")"
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

instance Prettyprint SitePath where
  pretty (SitePath name source target) = do
    pure $ name <> " : " <> source <> " -> " <> target

instance Prettyprint SiteDeclaration where
  pretty (SiteDeclaration name covers morphisms) = do
    coversStr <- pure $ unwords (intersperse ", " covers)
    morphismsStrs <- mapM pretty morphisms
    let morphismsStr = unlines morphismsStrs
    pure $ "site " <> name <> " covering (" <> coversStr <> "):\n" <> morphismsStr

instance Prettyprint EnumConstructor where
  pretty (EnumConstructor name fields) = do
    fieldsStrs <- mapM pretty fields
    let fieldsStr = unwords fieldsStrs
    pure $ name <> if null fields then "" else " " <> fieldsStr

instance Prettyprint EnumDeclaration where
  pretty (EnumDeclaration name ctors) = do
    ctorsStrs <- mapM pretty ctors
    let ctorsStr = unwords (intersperse ", " ctorsStrs)
    pure $ "enum " <> name <> " = " <> ctorsStr

instance Prettyprint StructDeclaration where
  pretty (StructDeclaration name fields) = do
    fieldsStrs <-
      mapM
        (\(fname, ftype) -> prettyField (fname, ftype))
        fields

    let fieldsStr = unlines fieldsStrs
    pure $ "record " <> name <> " {\n" <> fieldsStr <> "}"
   where
    prettyField (fname, ftype) = do
      ftypeStr <- pretty ftype
      pure $ fname <> ": " <> ftypeStr

instance Prettyprint FunctionParam where
  pretty (BinderParam binder) = pretty binder
  pretty (TypeParam name kind) = pure $ name <> " : " <> show kind

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
  pretty (EnumDecl enumDecl) = do
    enumDeclStr <- pretty enumDecl
    pure $ enumDeclStr <> ";"
  pretty (StructDecl structDecl) = do
    structDeclStr <- pretty structDecl
    pure $ structDeclStr <> ";"
  pretty (FunctionDecl funcDecl) = do
    funcDeclStr <- pretty funcDecl
    pure $ funcDeclStr <> ";"
  pretty (SiteDecl siteDecl) = do
    siteDeclStr <- pretty siteDecl
    pure $ siteDeclStr <> ";"
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
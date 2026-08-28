-- | Surface AST for the Ibis programming language.
module Ibis.Syntax.AST.Surface
  ( Literal (..)
  , Term (..)
  , Decl (..)
  , Program (..)
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

data Term
  = -- Universe levels (types are terms, universes classify types)
    Universe Int
  | Var String
  | Lit Literal
  | Unit
  | -- Dependent function types (Π-Types & Lambdas)
    Pi String Term Term -- Π(x : A). B
  | Lam String Term Term -- λ(x : A). e
  | App Term Term -- Unified term application (term or type)
  -- Dependent product types (Σ-Types & Tuples)
  | Sigma String Term Term -- Σ(x : A). B
  | Pair Term Term -- (p1, p2)
  | Fst Term -- fst p
  | Snd Term -- snd p
  -- Let bindings
  | Let String (Maybe Term) Term Term -- let x : A = e1 in e2
  | Ann Term Term -- e : A
  | -- Regular
    Unop Unop Term
  | Binop Binop Term Term
  | List [Term] -- [1, 2, 3]
  | Tuple [Term] -- (1, 2, 3)
  | If Term Term Term -- if cond then e1 else e2
  | For String Term Term -- for x in e1: e2
  | Match Term [(Pat, Term)] -- match e with | pat -> e
  deriving (Show, Eq)

---------------------------------------------
-- DECLARATION NODES
---------------------------------------------

-- A path in a topological site
data SitePath = SitePath String String String
  deriving (Show, Eq)

-- Function parameters can be binders or type parameters (polymorphism)
data FunctionParam
  = BinderParam Term
  | TypeParam String Kind
  deriving (Show, Eq)

-- Enum constructor with fields, e.g., Some Int, Error String
data EnumConstructor = EnumConstructor
  { enumCtorName :: String
  , enumCtorFields :: [Term]
  }
  deriving (Show, Eq)

data EnumDeclaration = EnumDeclaration
  { enumName :: String
  , enumConstructors :: [EnumConstructor]
  }
  deriving (Show, Eq)

data StructDeclaration = StructDeclaration
  { structName :: String
  , structFields :: [(String, Term)]
  }
  deriving (Show, Eq)

data FunctionDeclaration = FunctionDeclaration
  { funcName :: String
  , funcParams :: [FunctionParam]
  , funcReturnType :: Maybe Term
  , funcBody :: Term
  }
  deriving (Show, Eq)

data ClassDeclaration = ClassDeclaration
  { className :: String
  , classTypeParams :: [(String, Kind)]
  , classMethods :: [(String, Term)]
  }
  deriving (Show, Eq)

data ClassInstance = ClassInstance
  { instanceClassName :: String
  , instanceTypeArgs :: [Term]
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
  = ExprDecl Term
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
-- PATTERN NODES
---------------------------------------------

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

instance Prettyprint Term where
  pretty (Universe n) = pure $ "Type" <> show n
  pretty (Var name) = pure name
  pretty (Lit lit) = pretty lit
  pretty Unit = pure "()"
  pretty (Pi x a b) = do
    aStr <- pretty a
    bStr <- pretty b
    pure $ "Π(" <> x <> " : " <> aStr <> "). " <> bStr
  pretty (Lam x a e) = do
    aStr <- pretty a
    eStr <- pretty e
    pure $ "λ(" <> x <> " : " <> aStr <> "). " <> eStr
  pretty (App e1 e2) = do
    e1Str <- pretty e1
    e2Str <- pretty e2
    pure $ "(" <> e1Str <> " " <> e2Str <> ")"
  pretty _ = pure "<unimplemented prettyprint for this Term constructor>"

-- Add more cases for other Term constructors as needed.

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
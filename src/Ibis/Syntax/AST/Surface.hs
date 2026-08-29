{- |
  Module      : Ibis.Syntax.AST.Surface
  Description : Surface syntax AST for the Ibis language
-}
module Ibis.Syntax.AST.Surface
  ( Literal (..)
  , Term (..)
  , Decl (..)
  , Program (..)
  , Pat (..)

    -- * Declarations
  , Param (..)
  , StructDecl (..)
  , InductiveCtor (..)
  , InductiveDecl (..)
  , CoverRule (..)
  , SiteDeclaration (..)
  , SitePath (..)
  )
where

-- import Data.List (intersperse)
-- import Ibis.Prettyprint
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
  | If Term Term Term -- if cond then e1 else e2
  | For String Term Term -- for x in e1: e2
  | Match Term [(Pat, Term)] -- match e with | pat -> e
  | -- Topological Presheaf Primitives
    Site String -- A topological site
  | Cover Term Term -- Cover u v (u ⩿ v)
  | Sect Term Term -- Sect A u
  | Res Term Term -- ρ_{v,u} : Sect A v -> Sect A u
  | Ext Term Term Term -- e_{u,v} : Sect A u -> Sect A v
  deriving (Show, Eq)

---------------------------------------------
-- DECLARATION NODES
---------------------------------------------

data Param = Param
  { paramName :: String
  , paramType :: Term
  }
  deriving (Show, Eq)

-- A path in a topological site
data SitePath = SitePath String String String
  deriving (Show, Eq)

data StructDecl = StructDecl
  { structName :: String
  , structParams :: [Param]
  , structFields :: [(String, Term)]
  }
  deriving (Show, Eq)

data InductiveCtor = InductiveCtor
  { ctorName :: String
  , ctorType :: Term
  }
  deriving (Show, Eq)

data InductiveDecl = InductiveDecl
  { indName :: String
  , indParams :: [Param]
  , indArity :: Term -- Index arity + universe level
  , indConstructors :: [InductiveCtor]
  }
  deriving (Show, Eq)

data CoverRule = CoverRule
  { parentSite :: String
  , childSites :: [String]
  }
  deriving (Show, Eq)

data SiteDeclaration = SiteDeclaration
  { siteName :: String
  , siteCovers :: [CoverRule]
  , sitePaths :: [SitePath]
  }
  deriving (Show, Eq)

data Decl
  = TermDecl Term
  | StructDecl' StructDecl
  | InductiveDecl' InductiveDecl
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
  | PCtor String [Pat] -- Ctor x y
  | PPartition String Pat -- (x:xs)
  deriving (Show, Eq)

---------------------------------------------
-- PRETTYPRINT INSTANCES
---------------------------------------------

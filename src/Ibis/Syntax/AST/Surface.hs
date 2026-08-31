{- |
  Module      : Ibis.Syntax.AST.Surface
  Description : Surface syntax AST for the Ibis language
-}
module Ibis.Syntax.AST.Surface
  ( Literal (..)
  , SurfaceUniverse (..)
  , Term (..)
  , Decl (..)
  , Program (..)
  , Pat (..)
  , Tactic (..)

    -- * Declarations
  , Param (..)
  , InductiveCtor (..)
  , FunctionBody (..)
  , CoverRule (..)
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

data SurfaceUniverse
  = UnivName String -- A named universe (e.g., Type, Prop)
  | UnivLevel Int -- A universe level (e.g., Type 0, Type 1)
  deriving (Show, Eq)

data Term
  = -- Universe levels (types are terms, universes classify types)
    Universe SurfaceUniverse
  | -- Constants and variables
    Const String -- Constants (e.g., built-in functions, axioms)
  | Var String
  | Lit Literal
  | Unit
  | -- Dependent function types (Π-Types & Lambdas)
    Pi String Term Term -- Π(x : A). B
  | Lam String Term -- λ(x : A). e
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
  | -- Monadic constructs
    Do [Term] -- do { e1; e2; ... }
  | Bind String Term -- x <- e1
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

data Tactic
  = TacticIntro String -- intro x
  | TacticExact Term -- exact e
  | TacticApply Term -- apply e
  | TacticRfl -- rfl
  | TacticSimp Term -- simp e
  | TacticCases Term -- cases e
  | TacticInduction Term -- induction e
  | TacticBind String (Maybe Term) Term -- bind x : A = e
  | TacticHave String (Maybe Term) Term -- have x : A = e
  | TacticShow Term -- show e
  | TacticSorry -- sorry
  | TacticPathAcross Term Term -- path_across u v
  | TacticCovers Term Term -- covers u v
  | TacticRes Term Term -- res u v
  | TacticLan Term Term -- lan u v
  | TacticGlue Term Term Term Term -- glue u v secU secV
  deriving (Show, Eq)

data Param = Param
  { paramName :: String
  , paramType :: Term
  }
  deriving (Show, Eq)

data InductiveCtor = InductiveCtor
  { ctorName :: String
  , ctorType :: Term
  }
  deriving (Show, Eq)

data FunctionBody
  = SimpleBody Term -- A simple term body
  | TacticBody [Tactic] -- A tactic-based proof body
  deriving (Show, Eq)

-- Site declaration and covering rules
data CoverRule = CoverRule
  { parentSite :: String
  , childSites :: [String]
  }
  deriving (Show, Eq)

data Decl
  = TermDecl Term
  | StructDecl
      { structName :: String
      , structParams :: [Param]
      , structFields :: [(String, Term)]
      }
  | InductiveDecl
      { indName :: String
      , indParams :: [Param]
      , indArity :: Term
      , indConstructors :: [InductiveCtor]
      }
  | FunctionDecl
      { funcName :: String
      , funcParams :: [Param]
      , funcReturnType :: Term
      , funcBody :: FunctionBody
      }
  | SiteDecl
      { siteDeclName :: String
      , siteDeclCovers :: [CoverRule]
      }
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

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Co-inductive AST for Ibis, annotated with spatial coordinates ready for streaming
-- from a disk.
module Ibis.AST.CoAST where

import Data.Word (Word32, Word8)
import Ibis.AST.Surface (Literal (..), Pat (..))

-- | De Bruijn index for variables and type variables, relative
-- distance to binder (where 0 is the innermost lambda).
newtype Index = Index {unIndex :: Int}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, Num, Real, Integral)

-- | De Bruijn levels for rigid variables.
newtype Level = Level {unLevel :: Int}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Num, Enum)

-- | Spines are lists of values applied to a rigid or flexible head.
type Spine = [Value]

-- | Represents a position in a chunk, defined within the spatial universe (.itop chunk)
data ChunkPos = ChunkPos !Word32 !Word32 !Word32
  deriving (Eq, Show, Ord)

-- | Local coordinates within a chunk, represented as (x, y, z) offsets.
data LocalPos = LocalPos !Word8 !Word8 !Word8
  deriving (Eq, Show, Ord)

data SpatialCoord = SpatialCoord !ChunkPos !LocalPos
  deriving (Eq, Show, Ord)

-- | Spatial closure holding the coordinate cursor instead of a Haskell function
data SpatialClosure = SpatialClosure
  { closureEnv :: [Value]
  , closureBodyPos :: SpatialCoord
  }
  deriving (Eq, Show)

data CoTerm
  = Universe Int
  | Const String -- Constants (e.g., built-in functions, axioms)
  | MVar Int -- Meta-variable for unification
  | Var Index -- De Bruijn indexded variable
  | Lit Literal
  | Unit
  | -- Dependent Functions
    Pi (Maybe String) SpatialCoord SpatialCoord -- Π(A). B
  | Lam (Maybe String) SpatialCoord -- λ. body
  | App SpatialCoord SpatialCoord -- f x
  -- Dependent Products
  | Sigma (Maybe String) SpatialCoord SpatialCoord -- Σ(A). B
  | Pair SpatialCoord SpatialCoord -- (a, b)
  | Fst SpatialCoord -- fst p
  | Snd SpatialCoord -- snd p
  -- Language constructs
  | Let Index SpatialCoord SpatialCoord -- let x = e in body
  | Ann SpatialCoord SpatialCoord -- e : A
  | Match SpatialCoord [(Pat, SpatialCoord)]
  | -- Topological primitives
    Site Index -- A topological site
  | Cover SpatialCoord SpatialCoord -- Cover u v (u ⩿ v)
  | Sect SpatialCoord SpatialCoord -- Sect A u
  | Res SpatialCoord SpatialCoord SpatialCoord SpatialCoord SpatialCoord -- res u v a proof site
  | Ext SpatialCoord SpatialCoord SpatialCoord SpatialCoord SpatialCoord -- ext u v a proof site
  deriving (Eq, Show)

data CoDecl
  = CoDef
      { defName :: String
      , defType :: CoTerm
      , defBody :: CoTerm
      }
  | CoInductive
      { indName :: String
      , indType :: CoTerm
      , indConstructors :: [(String, CoTerm)]
      }

data Value
  = VUniverse Int
  | VConst String
  | VLit Literal
  | VPair Value Value
  | -- Closures hold spatial pointers
    VPi (Maybe String) Value SpatialClosure
  | VLam (Maybe String) Value SpatialClosure
  | VSigma (Maybe String) Value SpatialClosure
  | -- Topological primitives
    VSite Index
  | VCover Value Value
  | VSect Value Value
  | VNeutral Value Neutral
  deriving (Eq, Show)

-- Neutral terms represent computations that are "stuck" on a variable or a neutral term,
-- which cannot be further evaluated.
data Neutral
  = NVar Int
  | NApp Neutral Value
  | NFst Neutral
  | NSnd Neutral
  | NMatch Neutral [(Pat, Value)]
  | NRes Value Value Value Value Neutral
  | NExt Value Value Value Value Neutral
  deriving (Eq, Show)

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

-- | Represents the world in the Ibis compiler, consisting of a Grothendieck site
-- and a collection of world chunks, each with its own spatial coverage and data.
module Ibis.Compiler.World where

import Category.FiniteCover (FiniteCover)
import Category.Grothendieck (GrothendieckSite)
import Category.Presheaf.Type (Section)

import Ibis.AST.CoAST (ChunkPos (..))

-- | Represents a world in the Ibis compiler, consisting of a Grothendieck site
-- and a collection of voxel chunks, each with its own spatial coverage and data.
--
-- NOTE: cat is the category of spatial indices (e.g., 3D coordinates), c is the type of spatial index objects,
-- and val is the type of values stored in the voxel chunks (e.g., terrain data, block types, etc.).
data World cat (c :: cat) val = World
  { worldSite :: GrothendieckSite cat -- The Grothendieck site representing the spatial topology of the world
  , worldChunks :: [WorldChunk cat c val] -- List of voxel chunks in the world
  }

data WorldChunk cat (c :: cat) val = WorldChunk
  { chunkCoord :: !ChunkPos -- Coordinates of the chunk in the world
  , siteCoverage :: FiniteCover cat c -- Finite cover of the chunk's spatial index object in the Grothendieck site
  , chunkData :: Section val c -- Section of the presheaf representing the chunk's data
  }

data Region cat (c :: cat) val = Region
  { regionChunks :: [WorldChunk cat c val] -- List of voxel chunks in the region
  , regionChunkCount :: Int -- Number of chunks in the region
  }

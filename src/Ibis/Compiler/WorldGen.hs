{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | World-generation for Ibis.
--
-- World generation inspired by Minecraft's chunk based generation, where each chunk is a 16x16x16 cube of blocks.
-- Except:
-- 1. The world is a Grothendieck site with a topology defined by a sieve predicate, rather than a simple 3D grid.
-- 2. Each chunk is a presheaf over the site, with chunk data represented as sections of the presheaf.
-- 3. The world is infinite, but only a finite number of chunks are generated at any given time.
-- 4. The compiler has a render distance, what the fuck???
module Ibis.Compiler.WorldGen where

import Category.FiniteCover (CoveringArrow (..), FiniteCover (..))
import Category.Grothendieck (GrothendieckSite, Sieve (..), isCoveringSieve)
import Category.Presheaf.Type (Section)

import Data.Proxy (Proxy)

-- | Represents a world in the Ibis compiler, consisting of a Grothendieck site
-- and a collection of voxel chunks, each with its own spatial coverage and data.
--
-- NOTE: cat is the category of spatial indices (e.g., 3D coordinates), c is the type of spatial index objects,
-- and val is the type of values stored in the voxel chunks (e.g., terrain data, block types, etc.).
data World cat (c :: cat) val = World
  { worldSite :: GrothendieckSite cat -- The Grothendieck site representing the spatial topology of the world
  , worldChunks :: [VoxelChunk cat c val] -- List of voxel chunks in the world
  }

-- | A sector coordinate in the world, represented as a 3D integer vector (x, y, z).
type SectorCoord = (Int, Int, Int)

data VoxelChunk cat (c :: cat) val = VoxelChunk
  { chunkCoord :: SectorCoord
  , siteCoverage :: FiniteCover cat c
  , chunkData :: Section val c
  }

-- | Materialize an abstract sieve into a concrete finite cover by filtering the candidate covering arrows that satisfy
-- the sieve predicate.
materializeSieve
  :: Proxy c
  -- ^ Proxy tag for the spatial index object 'c'
  -> Int
  -- ^ Depth horizon for the finite cover
  -> [CoveringArrow cat c]
  -- ^ Candidate covering arrows into 'c'
  -> Sieve cat c
  -- ^ The sieve predicate defining the covering condition for 'c'
  -> FiniteCover cat c
  -- ^ The resulting finite cover with the filtered covering arrows
materializeSieve p sdepth candidates sieve =
  let valid = filter (\(CoveringArrow arr) -> sieve `contains` arr) candidates
   in FiniteCover
        { coverObject = p
        , depth = sdepth
        , coveringArrows = valid
        }

generateChunk
  :: GrothendieckSite cat
  -- ^ The Grothendieck site depicting the spatial topology of the world
  -> SectorCoord
  -- ^ The coordinates of the chunk to generate
  -> Proxy (c :: cat)
  -- ^ Proxy tag for the spatial index object 'c' representing the chunk
  -> Section val c
  -- ^ The section/payload type for the chunk data (the presheaf value)
  -> [CoveringArrow cat c]
  -- ^ Candidate covering arrows into the chunk's spatial index object 'c'
  -> Sieve cat c
  -- ^ The sieve predicate defining the covering condition for the chunk
  -> VoxelChunk cat c val
  -- ^ The generated voxel chunk with its spatial coverage and data
generateChunk site coord proxy payload candidates sieve =
  if isCoveringSieve site sieve
    then
      let finiteCover = materializeSieve proxy (computeDepth coord) candidates sieve
       in VoxelChunk
            { chunkCoord = coord
            , siteCoverage = finiteCover
            , chunkData = payload
            }
    else error "Sieve does not cover the chunk's spatial index object."
 where
  computeDepth :: SectorCoord -> Int
  computeDepth (_, y, _) = y -- Y coordinate is the depth
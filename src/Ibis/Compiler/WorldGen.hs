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
import Category.Presheaf.Arrow (Arrow (Comp, Inclusion))
import Category.Presheaf.Type (Section (Empty, Restrict))
import Data.Foldable (find)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word32)

import Ibis.AST.CoAST (ChunkPos (..), LocalPos)
import Ibis.Compiler.World (World (..), WorldChunk (..), chunkCoord, chunkData, worldChunks)

localRestriction :: LocalPos -> Arrow cat target c
localRestriction localPos = Inclusion localPos

localComposition :: LocalPos -> LocalPos -> Arrow cat target c
localComposition pos1 pos2 = Comp (Inclusion pos2) (Inclusion pos1)

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

chunkAt :: World cat c val -> ChunkPos -> Maybe (WorldChunk cat c val)
chunkAt world coord =
  let chunks = worldChunks world
   in find (\chunk -> chunkCoord chunk == coord) chunks

sectionIn
  :: World cat c val
  -- ^ The world containing the voxel chunks
  -> ChunkPos
  -- ^ The coordinates of the chunk to retrieve the section from
  -> Arrow cat target c
  -- ^ The sub-arrow representing inclusion of target into 'c'
  -> Maybe (Section val target)
  -- ^ The section of the presheaf over the target object, if the chunk exists
sectionIn world coord subArrow = do
  chunk <- chunkAt world coord
  pure $ Restrict subArrow (chunkData chunk)

-- | Generate a chunk in the world at the given coordinates
generateChunk
  :: GrothendieckSite cat
  -- ^ The Grothendieck site depicting the spatial topology of the world
  -> ChunkPos
  -- ^ The coordinates of the chunk to generate
  -> Proxy (c :: cat)
  -- ^ Proxy tag for the spatial index object 'c' representing the chunk
  -> Section val c
  -- ^ The section/payload type for the chunk data (the presheaf value)
  -> [CoveringArrow cat c]
  -- ^ Candidate covering arrows into the chunk's spatial index object 'c'
  -> Sieve cat c
  -- ^ The sieve predicate defining the covering condition for the chunk
  -> WorldChunk cat c val
  -- ^ The generated voxel chunk with its spatial coverage and data
generateChunk site coord proxy payload candidates sieve =
  if isCoveringSieve site sieve
    then
      let finiteCover = materializeSieve proxy (computeDepth coord) candidates sieve
       in WorldChunk
            { chunkCoord = coord
            , siteCoverage = finiteCover
            , chunkData = payload
            }
    else error "Sieve does not cover the chunk's spatial index object."
 where
  computeDepth :: ChunkPos -> Int
  computeDepth (ChunkPos _ y _) = fromIntegral y

-- | Generate the world with a given size (width, height, depth) and a Grothendieck site topology.
--
-- The world is infinitely expanding, width height and depth are the number of initial chunks to
-- generate (in each dimension)
generateWorld :: GrothendieckSite cat -> (Word32, Word32, Word32) -> World cat c val
generateWorld site (width, height, depth') =
  let coords =
        [ ChunkPos x y z
        | x <- [0 .. width - 1]
        , y <- [0 .. height - 1]
        , z <- [0 .. depth' - 1]
        ]
      chunks =
        map
          ( \coord ->
              -- Each chunk is generated with an empty section and a trivial sieve that covers
              -- the chunk's spatial index object.
              generateChunk site coord (Proxy :: Proxy c) Empty [] (Sieve $ \_ -> True)
          )
          coords
   in World
        { worldSite = site
        , worldChunks = chunks
        }
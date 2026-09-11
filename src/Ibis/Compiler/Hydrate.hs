{-# LANGUAGE ImportQualifiedPost #-}

-- | Hydrate a Grothendieck site by providing it with an inductive AST, turning
-- it into a co-inductive AST with spatial coordinates, ready for streaming.
module Ibis.Compiler.Hydrate where

import Control.Monad.State (State, get, gets, modify', put, runState)
import Data.Map qualified as M

import Ibis.AST.CoAST (ChunkPos (..), CoDecl (..), CoTerm (..), LocalPos (..), SpatialCoord (..))
import Ibis.AST.CoAST qualified as Co
import Ibis.AST.Surface (SurfaceUniverse (UnivLevel))
import Ibis.AST.Surface qualified as Surf

-- | Converts a 1D allocation slot index (0..4095) into a 3D local voxel coordinate
-- within a 16x16x16 chunk grid.
slotToLocalPos :: Int -> LocalPos
slotToLocalPos slot =
  let x = fromIntegral $ (slot `div` 16) `mod` 16
      y = fromIntegral $ (slot `div` 256) `mod` 16
      z = fromIntegral $ slot `mod` 16
   in LocalPos x y z

data HydrateState = HydrateState
  { hsChunkPos :: !ChunkPos
  -- ^ Active spatial chunk being populated
  , hsNextSlot :: !Int
  -- ^ Next available voxel slot offset within the current chunk
  , hsScopeMap :: ![String]
  -- ^ De Bruijn lexical environment tracking local variable bindings
  , hsNodeMap :: !(M.Map SpatialCoord CoTerm)
  -- ^ Primary AST map explicitly keyed by spatial coordinates
  }
  deriving (Show, Eq)

type Hydrate = State HydrateState

resolveVarIndex :: String -> [String] -> Co.Index
resolveVarIndex name env = case lookupName name env 0 of
  Just idx -> Co.Index idx
  Nothing -> Co.Index 0
 where
  lookupName :: String -> [String] -> Int -> Maybe Int
  lookupName _ [] _ = Nothing
  lookupName target (x : xs) depth
    | target == x = Just depth
    | otherwise = lookupName target xs (depth + 1)

-- | Allocates the next available slot for a new spatial coordinate in the active chunk.
allocCoordPtr :: Hydrate SpatialCoord
allocCoordPtr = do
  st <- get
  let slot = hsNextSlot st
      pos = slotToLocalPos slot
      coord = SpatialCoord (hsChunkPos st) pos
  put st{hsNextSlot = slot + 1}
  pure coord

-- | Records a hydrated co-inductive term into the spatial node map.
recordNode :: SpatialCoord -> CoTerm -> Hydrate ()
recordNode coord term = modify' $ \st ->
  st{hsNodeMap = M.insert coord term (hsNodeMap st)}

-- | Executes a hydration action within an extended lexical environment.
withScope :: String -> Hydrate a -> Hydrate a
withScope var action = do
  modify' $ \st -> st{hsScopeMap = var : hsScopeMap st}
  result <- action
  modify' $ \st -> st{hsScopeMap = drop 1 (hsScopeMap st)}
  pure result

resolveVar :: String -> Hydrate Co.Index
resolveVar name = do
  env <- gets hsScopeMap
  pure $ resolveVarIndex name env

--------------------------------------------------------------------------------
-- Hydration of Surface AST to CoAST

-- | Transforms an inductive surface AST term into a co-inductive spatial pointer ('SpatialCoord').
-- This recursively allocates child coordinates for sub-terms, constructs the necessary CoTerm node with
-- SpatialCoord pointers, records it into 'hsNodeMap', and returns its own coordinate.
--
-- We cannot make our CoTerm nodes directly recursive, because that would overflow GHCs stack/heap
-- so we use a spatial coordinate indirection (a pointer) to refer to child nodes.
hydrateTerm :: Surf.Term -> Hydrate SpatialCoord
hydrateTerm term = do
  -- Reserve a coordinate pointer for this node FIRST
  selfCoord <- allocCoordPtr

  -- Recursively hydrate children to get their SpatialCoords, then form the CoTerm
  coTerm <- case term of
    Surf.Universe (UnivLevel lvl) ->
      pure $ Co.Universe lvl
    Surf.Const name ->
      pure $ Co.Const name
    Surf.Var name -> do
      idx <- resolveVar name
      pure $ Co.Var idx
    Surf.Lit lit ->
      pure $ Co.Lit lit
    Surf.Unit ->
      pure Co.Unit
    Surf.App fn arg -> do
      fnCoord <- hydrateTerm fn
      argCoord <- hydrateTerm arg
      pure $ Co.App fnCoord argCoord
    Surf.Lam var body -> do
      bodyCoord <- withScope var $ hydrateTerm body
      pure $ Co.Lam (Just var) bodyCoord
    -- -- Dependent function types (Π-Types & Lambdas)
    Surf.Pi var dom cod -> do
      -- Pi expects: Maybe String -> SpatialCoord (domain) -> SpatialCoord (codomain)
      domCoord <- hydrateTerm dom
      codCoord <- withScope var $ hydrateTerm cod
      pure $ Co.Pi (Just var) domCoord codCoord

    -- Dependent product types (Σ-Types & Tuples)
    Surf.Sigma var dom cod -> do
      -- Sigma expects: Maybe String -> SpatialCoord (domain) -> SpatialCoord (codomain)
      domCoord <- hydrateTerm dom
      codCoord <- withScope var $ hydrateTerm cod
      pure $ Co.Sigma (Just var) domCoord codCoord
    Surf.Pair fst snd -> do
      fstCoord <- hydrateTerm fst
      sndCoord <- hydrateTerm snd
      pure $ Co.Pair fstCoord sndCoord
    Surf.Fst pair -> do
      pairCoord <- hydrateTerm pair
      pure $ Co.Fst pairCoord
    Surf.Snd pair -> do
      pairCoord <- hydrateTerm pair
      pure $ Co.Snd pairCoord
    -- Let bindings
    Surf.Let var _mty val body -> do
      -- Let expects: Index -> SpatialCoord (val) -> SpatialCoord (body)
      idx <- resolveVar var
      valCoord <- hydrateTerm val
      bodyCoord <- withScope var $ hydrateTerm body
      pure $ Co.Let idx valCoord bodyCoord
    Surf.Ann expr ty -> do
      exprCoord <- hydrateTerm expr
      tyCoord <- hydrateTerm ty
      pure $ Co.Ann exprCoord tyCoord
    Surf.Match scrutinee branches -> do
      scrutineeCoord <- hydrateTerm scrutinee
      branchCoords <-
        mapM
          ( \(pat, body) -> do
              bodyCoord <- hydrateTerm body
              pure (pat, bodyCoord)
          )
          branches
      pure $ Co.Match scrutineeCoord branchCoords
    _ -> error $ "Unsupported term for hydration: " ++ show term

  -- Record the CoTerm node at its pre-allocated coordinate
  recordNode selfCoord coTerm
  pure selfCoord

hydrateDecl :: Surf.Decl -> Hydrate CoDecl
hydrateDecl decl = error $ "Hydration of declarations not yet implemented: " ++ show decl

--------------------------------------------------------------------------------

runHydrate :: ChunkPos -> [Surf.Decl] -> ([CoDecl], M.Map SpatialCoord CoTerm)
runHydrate startChunk decls =
  let initialState =
        HydrateState
          { hsChunkPos = startChunk
          , hsNextSlot = 0
          , hsScopeMap = []
          , hsNodeMap = M.empty
          }
      (coDecls, finalState) = flip runState initialState $ mapM hydrateDecl decls
   in (coDecls, hsNodeMap finalState)
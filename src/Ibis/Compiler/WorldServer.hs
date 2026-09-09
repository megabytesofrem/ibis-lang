{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | World server for Ibis
--
-- Spawns a server that listens for requests to fetch or unload chunks and
-- manages loaded chunks in memory.
module Ibis.Compiler.WorldServer where

import Category.FiniteCover (CoveringArrow)
import Category.Grothendieck (GrothendieckSite, Sieve)
import Category.Presheaf.Type (Section (..))
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, ask, liftIO)
import Data.Proxy (Proxy (Proxy))

import Ibis.AST.CoAST (ChunkPos (ChunkPos))
import Ibis.Compiler.World (World (..), WorldChunk (..), lookupChunk)
import Ibis.Compiler.WorldGen (generateChunk)

-- | Chunk generator function
type ChunkGenerator cat (c :: cat) val = ChunkPos -> WorldChunk cat c val

-- | Server request parameterized over a category cat, a spatial index object type c,
-- and a value type val (the type of section values)
data ServerRequest cat c val where
  -- Fetch the cursor position: the "player"s current position in the world
  FetchCursor :: TMVar ChunkPos -> ServerRequest cat c val
  -- Fetch an existing chunk from the server's world state, or generate it if it doesn't exist.
  FetchChunk :: ChunkPos -> (TMVar (WorldChunk cat c val)) -> ServerRequest cat c val
  -- Unload a chunk from the server's world state, freeing up memory and resources.
  UnloadChunk :: ChunkPos -> ServerRequest cat c val

data ServerError
  = ChunkNotFound ChunkPos -- The requested chunk does not exist in the world
  | ChunkGenerationFailed ChunkPos -- Failed to generate the requested chunk
  | InvalidRequest String -- The request was invalid or malformed
  deriving (Show, Eq)

data ServerEnv cat (c :: cat) val = ServerEnv
  { serverWorld :: !(TVar (World cat c val)) -- The server's world state
  , serverCursor :: !(TVar ChunkPos) -- The current cursor position in the world
  , serverGenerator :: !(ChunkGenerator cat c val) -- The chunk generator function
  }

-- Our server is a ReaderT over IO, carrying the server context
type Server cat c val = ReaderT (ServerEnv cat c val) IO

popRequest :: TQueue (ServerRequest cat c val) -> Server cat c val (ServerRequest cat c val)
popRequest queue = liftIO . atomically $ readTQueue queue

reply :: TMVar a -> a -> Server cat c val ()
reply chan val = liftIO . atomically $ putTMVar chan val

initServer
  :: forall cat (c :: cat) val
   . GrothendieckSite cat
  -> Section val c
  -> [CoveringArrow cat c]
  -> Sieve cat c
  -> IO (ServerEnv cat c val)
initServer site initialSection coveringArrows sieve = do
  let world = World site []
  defaultCur <- newTVarIO (ChunkPos 0 0 0) -- Default cursor position at the origin
  worldVar <- newTVarIO world

  let generator pos = generateChunk site pos (Proxy :: Proxy c) initialSection coveringArrows sieve
  pure $ ServerEnv worldVar defaultCur generator

-- | Run the server, processing any and all requests
runServer :: TQueue (ServerRequest cat c val) -> Server cat c val ()
runServer requestQueue = forever $ do
  req <- popRequest requestQueue
  case req of
    FetchCursor replyVar -> do
      cursorResult <- fetchCursor
      case cursorResult of
        Right cursorPos -> reply replyVar cursorPos
        Left _err -> pure ()
    FetchChunk pos replyVar -> do
      result <- fetchChunk pos
      case result of
        Right chunk -> reply replyVar chunk
        Left _err -> pure ()
    UnloadChunk pos -> unloadChunk pos

fetchCursor :: Server cat c val (Either ServerError ChunkPos)
fetchCursor = do
  -- For now, we just return a fixed cursor position. In a real implementation, this would read
  -- a file cursor.dat, and return it
  pure $ Right (ChunkPos 0 0 0) -- Placeholder cursor position

-- | Fetch a chunk, generating it if it doesnt exist.
--
-- Returns a @WorldChunk@ if successful, or a @ServerError@ if the chunk could not be found or generated.
fetchChunk :: ChunkPos -> Server cat c val (Either ServerError (WorldChunk cat c val))
fetchChunk pos = do
  ServerEnv worldVar _cursor generator <- ask

  -- Read the current world state
  world <- liftIO $ readTVarIO worldVar
  case lookupChunk pos world of
    Just chunk -> pure $ Right chunk -- Chunk already exists, return it
    Nothing -> do
      -- Chunk does not exist, generate it
      let newChunk = generator pos
      pure $ Right newChunk

-- | Unload a chunk from the server, removing it from memory
unloadChunk :: ChunkPos -> Server cat c val ()
unloadChunk pos = do
  ServerEnv worldVar _cursor _generator <- ask
  liftIO . atomically $ do
    world <- readTVar worldVar
    let updatedChunks = filter (\chunk -> chunkCoord chunk /= pos) (worldChunks world)
    writeTVar worldVar world{worldChunks = updatedChunks}
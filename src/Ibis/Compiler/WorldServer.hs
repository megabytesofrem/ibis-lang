{-# LANGUAGE GADTs #-}

module Ibis.Compiler.WorldServer where

import Control.Concurrent.STM

import Ibis.AST.CoAST (LocalPos)
import Ibis.Compiler.World (WorldChunk)

data ServerRequest where
  -- Fetch an existing chunk from the server's world state, or generate it if it doesn't exist.
  FetchChunk :: LocalPos -> (TMVar (WorldChunk cat c val)) -> ServerRequest
  -- Unload a chunk from the server's world state, freeing up memory and resources.
  UnloadChunk :: LocalPos -> ServerRequest

initServer :: IO (TQueue ServerRequest)
initServer = newTQueueIO
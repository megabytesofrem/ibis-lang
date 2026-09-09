{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)

-- Category Theory Modules
import Category.FiniteCover (CoveringArrow (..))
import Category.Grothendieck
  ( GrothendieckSite
  , Sieve (..)
  , mkSite
  )
import Category.Presheaf.Arrow (Arrow (..))
import Category.Presheaf.Type (Section (..))

-- Compiler World Server & Visual Debugger Engine Modules
import Ibis.Compiler.Debugger.Server (startDebugger)
import Ibis.Compiler.WorldServer (initServer, runServer)

-- -----------------------------------------------------------------------------
-- 1. Real Topological Site & Sieve Materialization
-- -----------------------------------------------------------------------------

-- | Construct a canonical Grothendieck site where every sieve acts as a valid cover.
-- This satisfies base-change stability, local character, and maximal sieve axioms.
canonicalSite :: forall cat. GrothendieckSite cat
canonicalSite =
  fromMaybe (error "[Ibis Fatal] Failed to validate Grothendieck site topology axioms.") $
    mkSite (\(_ :: Sieve cat c) -> True)

-- | Construct the universal identity sieve on an index object 'c'.
-- Contains every arrow in the category that factors into 'c'.
universalSieve :: forall cat (c :: cat). Sieve cat c
universalSieve = Sieve $ \(_ :: Arrow cat d c) -> True

-- | Primary section value payload representing the core IR root
initialSectionPayload :: forall cat (c :: cat). Section String c
initialSectionPayload = Base "Ibis.AST.CoAST.RootSection"

-- | Primary covering arrow set (Identity inclusion arrow into root object c)
initialCoveringArrows :: forall cat (c :: cat). [CoveringArrow cat c]
initialCoveringArrows = [CoveringArrow Id]

-- -----------------------------------------------------------------------------
-- 2. Executable Entry Point
-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=================================================================="
  putStrLn "   Ibis Compiler Runtime & Spatial Grothendieck Debugger Engine   "
  putStrLn "=================================================================="

  putStrLn "[Ibis Engine] Materializing Grothendieck site topology from axioms..."

  -- 1. Initialize the WorldServer state environment with verified topological types
  env <- initServer canonicalSite initialSectionPayload initialCoveringArrows universalSieve

  -- 2. Allocate the lock-free STM request queue
  requestQueue <- newTQueueIO

  -- 3. Spawn the background WorldServer evaluation loop on a dedicated GHC green thread
  putStrLn "[Ibis Engine] Forking concurrent WorldServer polling loop..."
  _ <- forkIO $ runReaderT (runServer requestQueue) env

  -- 4. Bind TCP 0.0.0.0:25545 and run the spatial visual debugger on the main thread
  putStrLn "[Ibis Debugger] Binding socket to 0.0.0.0:25545..."
  putStrLn "[Ibis Debugger] Connect via Minecraft 1.20.1 (Protocol 763) at localhost:25545"

  startDebugger env requestQueue
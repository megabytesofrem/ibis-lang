module Ibis.Parser.Netlist where

import Ibis.Parser.Lexer (Parser, pIdent, symbol)

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (float)

-- | A netlist for a universe, containing many worlds
data UniverseNetlist
  = UniverseNetlist
  { unlInterface :: [UniverseNetlistForwardDecl]
  }

-- | A netlist for a world, containing the world name and a list of
-- forward declarations for the world interface.
data WorldNetlist
  = WorldNetlist
  { wnlName :: String
  , wnlVersion :: Float
  , wnlInterface :: [NetlistForwardDecl]
  }

-- | A netlist forward declaration for a universe
data UniverseNetlistForwardDecl
  = UniverseNetlistForwardDecl
  { unlfdName :: String
  , unlfdMapping :: [(String, String)] -- (mappingName, worldName)
  }

-- | A netlist forward declaration
data NetlistForwardDecl
  = NetlistForwardDecl
  { nfdName :: String
  , nfdParams :: [String]
  }

pForwardDecl :: Parser NetlistForwardDecl
pForwardDecl = do
  name <- pIdent
  _ <- symbol ":"
  params <- pType `sepBy1` symbol "->"
  pure $ NetlistForwardDecl name params
 where
  pType :: Parser String
  pType = pIdent

-- Parser for a universe netlist (name.iuniv):
-- interface
--  world <mappingName> : <worldName>
-- end
pUniverseNetlist :: Parser UniverseNetlist
pUniverseNetlist = do
  netlistItems <-
    between (symbol "interface") (symbol "end") $
      many pNetlistItem
  pure $ UniverseNetlist netlistItems
 where
  pNetlistItem :: Parser UniverseNetlistForwardDecl
  pNetlistItem = do
    name <- pIdent
    (mappingName, worldName) <- do
      mappingName <- pIdent
      _ <- symbol ":"
      worldName <- pIdent
      pure (mappingName, worldName)
    pure $ UniverseNetlistForwardDecl name [(mappingName, worldName)]

-- Parser for a world netlist (name.iworld):
-- world <name>
-- version <version>
-- interface
--  <name> : <a> -> <b> -> <c>
-- end
pWorldNetlist :: Parser WorldNetlist
pWorldNetlist = do
  _ <- symbol "world"
  name <- pIdent
  _ <- symbol "version"
  version <- float

  netlistItems <-
    between (symbol "interface") (symbol "end") $
      many pForwardDecl
  pure $ WorldNetlist name version netlistItems
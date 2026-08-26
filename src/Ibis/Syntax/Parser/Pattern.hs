module Ibis.Syntax.Parser.Pattern (pPattern) where

import Ibis.Syntax.AST (Pat (..))
import Ibis.Syntax.Parser.Lexer (Parser, enclosed, pCtorName, pIdent, pLiteral, symbol)
import Text.Megaparsec

ctorPattern :: Parser Pat
ctorPattern = do
  name <- pCtorName
  patterns <- many pPattern
  pure $ PCtor name patterns

tuplePattern :: Parser Pat
tuplePattern = do
  patterns <- enclosed '(' ')' $ pPattern `sepBy` symbol ","
  case patterns of
    [p] -> pure p -- Single pattern in parentheses is just that pattern
    ps -> pure $ PTuple ps -- A tuple pattern

listPattern :: Parser Pat
listPattern = do
  patterns <- enclosed '[' ']' $ pPattern `sepBy` symbol ","
  pure $ PList patterns

partitionPattern :: Parser Pat
partitionPattern = do
  first <- pIdent <|> symbol "_"
  _ <- symbol "::"
  rest <- pPattern
  pure $ PPartition first rest

capturePattern :: Parser Pat
capturePattern = PCapture <$> pIdent

wildcardPattern :: Parser Pat
wildcardPattern = PWildcard <$ symbol "_"

pPattern :: Parser Pat
pPattern =
  choice
    [ PLit <$> pLiteral
    , try ctorPattern
    , try wildcardPattern
    , try capturePattern
    , try partitionPattern
    , try tuplePattern
    , try listPattern
    ]
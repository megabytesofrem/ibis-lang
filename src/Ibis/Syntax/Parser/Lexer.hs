{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Syntax.Parser.Lexer
  ( Parser

    -- * Lexing utilities
  , sc
  , lexeme
  , symbol
  , enclosed
  , enclosedStr
  , alternativeSym
  , parens
  , brackets
  , braces

    -- * Core parsers
  , pIdentImpl
  , pCtorNameImpl
  , pIdent
  , pCtorName
  , pQualifiedName
  , pLiteral
  )
where

import Data.List (intersperse)
import Data.Void (Void)
import Ibis.Syntax.AST (Literal (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- Parser type alias for convenience
type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

enclosed :: Char -> Char -> Parser a -> Parser a
enclosed open close = between (symbol [open] >> sc) (symbol [close] >> sc)

enclosedStr :: String -> String -> Parser a -> Parser a
enclosedStr open close = between (symbol open >> sc) (symbol close >> sc)

alternativeSym :: String -> String -> Parser String
alternativeSym s1 s2 = symbol s1 <|> symbol s2

parens :: Parser a -> Parser a
parens = enclosed '(' ')'

brackets :: Parser a -> Parser a
brackets = enclosed '[' ']'

braces :: Parser a -> Parser a
braces = enclosed '{' '}'

---------------------------------------------

-- List of reserved words in the language
reservedWords :: [String]
reservedWords =
  [ "∀"
  , "forall"
  , "type"
  , "record"
  , "class"
  , "impl"
  , "def"
  , "if"
  , "then"
  , "else"
  , "for"
  , "match"
  , "with"
  , "let"
  , "in"
  , "do"
  , "end"
  , "ret"
  ]

-- List of reserved primitive types in the language
primTypes :: [String]
primTypes =
  [ "Int"
  , "Uint"
  , "Float"
  , "Bool"
  , "String"
  , "Char"
  , "Unit"
  ]

isPrim :: String -> Bool
isPrim name = name `elem` primTypes

pIdentImpl :: Parser String
pIdentImpl = lexeme . try $ do
  firstChar <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  let ident = firstChar : rest
  if ident `elem` reservedWords || isPrim ident
    then fail $ "Reserved word or primitive type: " ++ ident
    else pure ident

pCtorNameImpl :: Parser String
pCtorNameImpl = do
  firstChar <- upperChar
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  let ident = firstChar : rest
  if ident `elem` reservedWords || isPrim ident
    then fail $ "Reserved word or primitive type: " ++ ident
    else pure ident

pIdent :: Parser String
pIdent = try pIdentImpl

-- Parse an uppercase constructor name, e.g. Some, Error
pCtorName :: Parser String
pCtorName = lexeme . try $ pCtorNameImpl

-- Parse a fully qualified name, e.g. Module.Submodule.TypeName
pQualifiedName :: Parser String
pQualifiedName = lexeme . try $ do
  parts <- some (try $ pCtorNameImpl <* symbol ".")
  lastPart <- pIdentImpl
  pure $ concat (intersperse "." (parts ++ [lastPart]))

pIntegerLit :: Parser Integer
pIntegerLit = lexeme L.decimal

pFloatLit :: Parser Double
pFloatLit = lexeme L.float

pStringLit :: Parser String
pStringLit = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

pLiteral :: Parser Literal
pLiteral =
  choice
    [ LitInt <$> pIntegerLit
    , LitFloat <$> pFloatLit
    , LitBool True <$ symbol "true"
    , LitBool False <$ symbol "false"
    , LitString <$> pStringLit
    ]

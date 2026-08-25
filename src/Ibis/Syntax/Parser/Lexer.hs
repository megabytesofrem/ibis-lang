{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Syntax.Parser.Lexer
  ( Parser
  , sc
  , lexeme
  , symbol
  , enclosed
  , enclosedStr
  , alternativeSym
  , pIdentImpl
  , pCtorNameImpl
  )
where

import Data.Void (Void)
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

---------------------------------------------

-- List of reserved words in the language
reservedWords :: [String]
reservedWords =
  [ "∀"
  , "forall"
  , "type"
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
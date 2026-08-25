{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Syntax.Parser.Decl
  ( -- * Parsers
    pIdent
  , pCtorName
  , pQualifiedName
  , pLiteral
  , pType
  , pExpr
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List (intersperse)
import Ibis.Syntax.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Ibis.Syntax.Parser.Lexer (Parser, alternativeSym, enclosed, lexeme, pCtorNameImpl, pIdentImpl, symbol)

-- Parsers for types
pType :: Parser Ty
pType = try pForAll <|> pArrowTy

-- An atomic type: either a primitive type or a type variable
pAtomicTy :: Parser Ty
pAtomicTy =
  choice
    [ TInt <$ symbol "Int"
    , TFloat <$ symbol "Float"
    , TBool <$ symbol "Bool"
    , TString <$ symbol "String"
    , TUnit <$ symbol "Unit"
    , TVar <$> pIdentImpl
    , parens pType
    ]
 where
  parens = enclosed '(' ')'

-- Type application: Maybe Int, List String, etc.
pAppTy :: Parser Ty
pAppTy =
  choice
    [ try $ do
        ctor <- pCtorName
        args <- many pAtomicTy
        case args of
          [] -> pure $ TCons ctor []
          _ -> pure $ foldl' TApp (TCons ctor []) args
    , try $ do
        var <- pIdent
        args <- many pAtomicTy
        pure $ foldl' TApp (TVar var) args
    , pAtomicTy
    ]

-- Forall types: forall a. a -> a
pForAll :: Parser Ty
pForAll = do
  _ <- symbol "forall" <|> symbol "∀"
  tvars <- some pIdentImpl
  _ <- symbol "."
  ty <- pArrowTy
  pure $ foldr TForall ty tvars

-- Arrow types: Int -> Int, Maybe Int -> Bool
pArrowTy :: Parser Ty
pArrowTy = do
  arg <- pAppTy
  rest <- optional (alternativeSym "->" "→" >> pArrowTy)
  pure $ case rest of
    Just r -> TFunc arg r
    Nothing -> arg

---------------------------------------------

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

---------------------------------------------
-- Expression parsers

pBinder :: Parser Binder
pBinder = do
  name <- pIdent
  mty <- optional (symbol ":" >> pType)
  pure $ Binder name mty

pListExpr :: Parser Expr
pListExpr = enclosed '[' ']' (EList <$> pExpr `sepBy` symbol ",")

pLet :: Parser Expr
pLet = do
  _ <- symbol "let"
  binder <- pBinder
  _ <- symbol "="
  value <- pExpr
  _ <- symbol "in"
  ELet binder value <$> pExpr

pApply :: Parser Expr
pApply = do
  func <- pTerm
  args <- many pTerm
  pure $ foldl' EApp func args

pTerm :: Parser Expr
pTerm =
  choice
    [ ELit <$> pLiteral
    , EVar <$> pIdent
    , pLet
    , pListExpr
    , exprOrTuple
    ]
 where
  exprOrTuple = enclosed '(' ')' $ do
    exprs <- pExpr `sepBy` symbol ","
    case exprs of
      [] -> pure EUnit -- empty tuple is unit
      [e] -> pure e -- parenthesized expression
      es -> pure $ ETuple es

pExpr :: Parser Expr
pExpr = makeExprParser pApply operatorTable

-- Operator precedence table

prefix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

infixL :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
infixL name f = InfixL (f <$ symbol name)

infixR :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
infixR name f = InfixR (f <$ symbol name)

infixN :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
infixN name f = InfixN (f <$ symbol name)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ prefix "-" (EUnop Negate)
    , prefix "not" (EUnop Not)
    ]
  ,
    [ infixL "*" (EBinop Mul)
    , infixL "/" (EBinop Div)
    ]
  ,
    [ infixL "+" (EBinop Add)
    , infixL "-" (EBinop Sub)
    ]
  ,
    [ infixN "==" (EBinop Eq)
    , infixN "!=" (EBinop Neq)
    , infixN "<" (EBinop Lt)
    , infixN ">" (EBinop Gt)
    , infixN "<=" (EBinop Leq)
    , infixN ">=" (EBinop Geq)
    ]
  , [infixL "and" (EBinop And)]
  , [infixL "or" (EBinop Or)]
  ]

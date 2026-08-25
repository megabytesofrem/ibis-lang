{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Syntax.Parser.Decl
  ( -- * Parsers
    pQualifiedName
  , pLiteral
  , pType
  , pExpr
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Ibis.Syntax.AST
import Text.Megaparsec

import Ibis.Syntax.Parser.Lexer
import Ibis.Syntax.Parser.Pattern (pPattern)

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

pIf :: Parser Expr
pIf = do
  _ <- symbol "if"
  cond <- pExpr
  _ <- symbol "then"
  thenExpr <- pExpr
  _ <- symbol "else"
  elseExpr <- pExpr
  pure $ EIf cond thenExpr elseExpr

pFor :: Parser Expr
pFor = do
  _ <- symbol "for"
  binder <- pBinder
  _ <- symbol "in"
  collection <- pExpr
  _ <- symbol ":"
  body <- pExpr
  pure $ EFor binder collection body

pMatchArm :: Parser (Pat, Expr)
pMatchArm = do
  pat <- pPattern
  _ <- alternativeSym "->" "→"
  expr <- pExpr
  pure (pat, expr)

pMatch :: Parser Expr
pMatch = do
  _ <- symbol "match"
  expr <- pExpr
  _ <- symbol "with"
  firstArm <- symbol "|" >> pMatchArm
  restArms <- many (symbol "|" >> pMatchArm)
  let arms = firstArm : restArms
  pure $ EMatch expr arms

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
    , pIf
    , pFor
    , pMatch
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

---------------------------------------------
-- Declaration parsers
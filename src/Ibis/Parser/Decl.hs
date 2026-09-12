module Ibis.Parser.Decl where

import Text.Megaparsec

import Ibis.AST.Surface
import Ibis.Parser.Lexer (Parser, pIdent, symbol)
import Ibis.Parser.Term (pExpr, telescope)

-- Parse do notation:
--  do { e1; e2; ... }
--  do
--   e1
--   e2
pDoNotation :: Parser Term
pDoNotation = do
  _ <- symbol "do"
  _ <- optional (symbol "{")
  terms <- pMonadicTerm `sepBy` (symbol ";" <|> symbol "\n")
  _ <- optional (symbol "}")
  pure $ Do terms

pMonadicTerm :: Parser Term
pMonadicTerm = try pMonadicBind <|> pExpr

-- Parse a monadic bind: x <- e1
pMonadicBind :: Parser Term
pMonadicBind = do
  name <- pIdent
  _ <- symbol "<-"
  expr <- pExpr
  pure $ Bind name expr

-- Parse a covering rule: cover @Parent has {@Child1, @Child2, ...}
pCoveringRule :: Parser CoverRule
pCoveringRule = do
  _ <- symbol "cover"
  parent <- symbol "@" *> pIdent
  _ <- symbol "has"
  children <-
    between (symbol "{") (symbol "}") $
      (symbol "@" *> pIdent) `sepBy` symbol ","
  pure $ CoverRule parent children

-- Parse a site declaration:
-- site SiteName where
--   cover @J has {@Child1, @Child2, ...}
--   cover @K has {@Child3, @Child4, ...}
pSiteDeclaration :: Parser Decl
pSiteDeclaration = do
  _ <- symbol "site"
  name <- pIdent
  _ <- symbol "where"
  covers <- many pCoveringRule
  pure $ SiteDecl name covers

-- Parse a struct declaration:
-- struct Buffer (site : Site) where
--   capacity : Nat
--   data : Array capacity
pStructDeclaration :: Parser Decl
pStructDeclaration = do
  _ <- symbol "struct"
  name <- pIdent
  params <- telescope
  _ <- symbol "where"
  fields <- many field
  pure $ StructDecl name params fields
 where
  field :: Parser (String, Term)
  field = do
    fieldName <- pIdent
    _ <- symbol ":"
    fieldType <- pExpr
    pure (fieldName, fieldType)

-- Parse an inductive constructor:
--   Nil : Vect A 0
--   Cons : (x : A) -> (xs : Vect A n) -> Vect A (n + 1)
pInductiveConstructor :: Parser InductiveCtor
pInductiveConstructor = do
  name <- pIdent
  _ <- symbol ":"
  typ <- pExpr
  pure $ InductiveCtor name typ

-- Parse an inductive declaration:
-- inductive Vect (A : Type 0) : Nat -> Type 0 where
--   Nil : Vect A 0
--   Cons : (x : A) -> (xs : Vect A n) -> Vect A (n + 1)
pInductiveDeclaration :: Parser Decl
pInductiveDeclaration = do
  _ <- symbol "inductive"
  name <- pIdent
  params <- telescope -- Consumes fixed parameters like (A : Type 0)
  _ <- symbol ":"
  arity <- pExpr -- Consumes index arity + universe like Nat -> Type 0
  _ <- symbol "where"
  ctors <- many pInductiveConstructor
  pure $ InductiveDecl name params arity ctors

pFunctionBody :: Parser FunctionBody
pFunctionBody = SimpleBody <$> pExpr

-- Parse a forward declaration:
--   foo : Int -> Int
pForwardDeclaration :: Parser ForwardDecl
pForwardDeclaration = do
  name <- pIdent
  _ <- symbol ":"
  typ <- pExpr
  pure $ ForwardDecl name [] (Just typ)

-- Parse a function definition:
--   foo' a b = a + b
pFunctionDeclaration :: Parser Decl
pFunctionDeclaration =
  do
    name <- pIdent
    params <- many pIdent
    _ <- symbol "="
    body <- pExpr
    pure $ FunctionDecl name params Nothing (Just $ SimpleBody body)

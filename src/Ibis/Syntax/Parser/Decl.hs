{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ibis.Syntax.Parser.Decl
  ( -- * Parsers
    pQualifiedName
  , pLiteral
  , pType
  , parseExpr
  , parseDecl
  , parseProgram
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List (intercalate)
import Text.Megaparsec

import Ibis.Syntax.AST
import Ibis.Syntax.AST.Kind (Kind (KStar))
import Ibis.Syntax.Parser.Lexer
import Ibis.Syntax.Parser.Pattern (pPattern)

---------------------------------------------
-- TYPE PARSERS
---------------------------------------------

-- Parse a type
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
    , (\name -> TVar name Nothing) <$> pIdentImpl
    , parens pType
    ]

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
        pure $ foldl' TApp (TVar var Nothing) args
    , pAtomicTy
    ]

-- Forall types: forall a. a -> a
pForAll :: Parser Ty
pForAll = do
  _ <- symbol "forall" <|> symbol "∀"
  tvars <- some pIdentImpl
  _ <- symbol "."
  ty <- pArrowTy
  pure $ foldr (\name acc -> TForall name KStar acc) ty tvars

-- Arrow types: Int -> Int, Maybe Int -> Bool
pArrowTy :: Parser Ty
pArrowTy = do
  arg <- pAppTy
  rest <- optional (alternativeSym "->" "→" >> pArrowTy)
  pure $ case rest of
    Just r -> TFunc arg r
    Nothing -> arg

---------------------------------------------
-- EXPRESSION PARSERS
---------------------------------------------

pBinder :: Parser Binder
pBinder = do
  name <- pIdent
  mty <- optional (symbol ":" >> pType)
  pure $ Binder name mty

pListExpr :: Parser Expr
pListExpr = brackets (EList <$> parseExpr `sepBy` symbol ",")

-- Let expression: let exp = ... in body
pLet :: Parser Expr
pLet = do
  _ <- symbol "let"
  binder <- pBinder
  _ <- symbol "="
  value <- parseExpr
  _ <- symbol "in"
  ELet binder value <$> parseExpr

-- If expression: if cond then e1 else e2
pIf :: Parser Expr
pIf = do
  _ <- symbol "if"
  cond <- parseExpr
  _ <- symbol "then"
  thenExpr <- parseExpr
  _ <- symbol "else"
  elseExpr <- parseExpr
  pure $ EIf cond thenExpr elseExpr

-- For expression: for binder in xs: body
pFor :: Parser Expr
pFor = do
  _ <- symbol "for"
  binder <- pBinder
  _ <- symbol "in"
  collection <- parseExpr
  _ <- symbol ":"
  body <- parseExpr
  pure $ EFor binder collection body

pMatchArm :: Parser (Pat, Expr)
pMatchArm = do
  pat <- pPattern
  _ <- alternativeSym "->" "→"
  expr <- parseExpr
  pure (pat, expr)

-- Match expression: match expr with | pat1 -> expr1 | pat2 -> expr2
pMatch :: Parser Expr
pMatch = do
  _ <- symbol "match"
  expr <- parseExpr
  _ <- symbol "with"
  firstArm <- (optional (symbol "|") >> pMatchArm)
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
  exprOrTuple = parens $ do
    exprs <- parseExpr `sepBy` symbol ","
    case exprs of
      [] -> pure EUnit -- empty tuple is unit
      [e] -> pure e -- parenthesized expression
      es -> pure $ ETuple es

parseExpr :: Parser Expr
parseExpr = makeExprParser pApply operatorTable

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
-- DECLARATION PARSERS
---------------------------------------------

pField :: Parser (String, Ty)
pField = do
  name <- pIdent
  _ <- symbol ":"
  ty <- pType
  pure (name, ty)

-- Parse a struct declaration:
--  record Foo where
--   field1: Int,
--   field2: String
pStructDecl :: Parser Decl
pStructDecl = do
  _ <- symbol "struct"
  name <- pCtorName
  _ <- symbol "where"
  fields <- pField `sepBy1` symbol ","
  pure $ StructDecl $ StructDeclaration name fields

-- Parse an enum declaration:
-- enum Option {α : ∀α. Option α} where
--   | Some α
--   | None
pEnumDecl :: Parser Decl
pEnumDecl = do
  _ <- symbol "enum"
  name <- pCtorName
  _ <- symbol "where"
  constructors <- some parseConstructor
  pure $ EnumDecl $ EnumDeclaration name constructors
 where
  parseConstructor :: Parser EnumConstructor
  parseConstructor = do
    _ <- symbol "|"
    ctorName <- pCtorName
    ctorFields <- many pType
    pure $ EnumConstructor ctorName ctorFields

pModuleName :: Parser String
pModuleName = do
  parts <- pIdent `sepBy1` symbol "."
  pure $ intercalate "." parts

pAliasedImport :: Parser Decl
pAliasedImport = do
  _ <- symbol "import"
  moduleName <- pModuleName
  _ <- symbol "as"
  alias <- pIdent
  pure $ ImportDecl moduleName (Just alias)

pExposingImport :: Parser Decl
pExposingImport = do
  _ <- symbol "import"
  moduleName <- pModuleName
  _ <- symbol "exposing"
  names <- parens (pIdent `sepBy1` symbol ",")
  pure $ ImportDeclExposing moduleName names

pWholeImport :: Parser Decl
pWholeImport = do
  _ <- symbol "import"
  moduleName <- pModuleName
  pure $ ImportDecl moduleName Nothing

-- Parse an import declaration, which can be either aliased, exposing, or whole
-- - Aliased import: import Module [as Alias]
-- - Exposing import: import Module exposing (name1, name2)
-- - Whole import: import Module
pImportDecl :: Parser Decl
pImportDecl =
  try pAliasedImport
    <|> try pExposingImport
    <|> try pWholeImport

pFunctionParam :: Parser FunctionParam
pFunctionParam = BinderParam <$> pBinder

-- Parse a function declaration:
-- def siteBound {@Site} (ptr: Ptr Int {@Site}) : Int := ...
-- def add (a: Int) (b: Int): Int := a + b
-- def id {α : ∀α.α} (a: α): α := a
pFunctionDecl :: Parser Decl
pFunctionDecl = do
  _ <- symbol "def"
  funcName <- pIdent
  paramGroups <- many (parens (pFunctionParam `sepBy` symbol ","))
  let funcParams = concat paramGroups
  returnType <- optional (symbol ":" >> pType)
  _ <- symbol ":="
  body <- parseExpr
  pure $
    FunctionDecl $
      FunctionDeclaration
        { funcName
        , funcParams
        , funcReturnType = returnType
        , funcBody = body
        }

pSiteCover :: Parser String
pSiteCover = symbol "~" *> pIdent

pSitePath :: Parser SiteMorphism
pSitePath = do
  name <- pIdent
  _ <- symbol ":"
  source <- pSiteCover
  _ <- alternativeSym "->" "→"
  target <- pSiteCover
  pure $ SiteMorphism name source target

-- Parse a topological site declaration:
-- site MySite where
--   covers: [~a, ~b]
--   paths:
--     f: ~a -> ~b
--     g: ~b -> ~a
pSiteDecl :: Parser Decl
pSiteDecl = do
  _ <- symbol "site"
  siteName <- pIdent
  _ <- symbol "where"

  _ <- symbol "covers:"
  siteCovers <- brackets (pSiteCover `sepBy` symbol ",")
  _ <- symbol "paths:"
  sitePaths <- many pSitePath
  pure $ SiteDecl $ SiteDeclaration{siteName, siteCovers, sitePaths}

parseDecl :: Parser Decl
parseDecl =
  choice
    [ try pEnumDecl
    , try pStructDecl
    , try pFunctionDecl
    , try pImportDecl
    , try pSiteDecl
    ]

parseProgram :: Parser [Decl]
parseProgram = many (parseDecl <* optional (symbol ";"))
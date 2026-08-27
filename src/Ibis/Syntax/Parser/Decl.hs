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
import Ibis.Syntax.AST.Kind (Kind (KArrow, KStar))
import Ibis.Syntax.Parser.Lexer
import Ibis.Syntax.Parser.Pattern (pPattern)

---------------------------------------------
-- TYPE PARSERS
---------------------------------------------

-- Parse a type
pType :: Parser Ty
pType = try pForAll <|> pArrowTy

pAtomicKind :: Parser Kind
pAtomicKind =
  choice
    -- The kind of types
    [ KStar <$ symbol "Type"
    , parens pKind
    ]

pKindArrow :: Parser Kind
pKindArrow = do
  argKind <- pAtomicKind
  rest <- optional (alternativeSym "->" "→" >> pKindArrow)
  pure $ case rest of
    Just retKind -> KArrow argKind retKind
    Nothing -> argKind

pKind :: Parser Kind
pKind = pKindArrow

-- An atomic type: either a primitive type or a type variable
pAtomicTy :: Parser Ty
pAtomicTy =
  choice
    [ TInt <$ symbol "Int"
    , TFloat <$ symbol "Float"
    , TBool <$ symbol "Bool"
    , TString <$ symbol "String"
    , TUnit <$ symbol "Unit"
    , try (parens pAnnotatedTVar)
    , (\name -> TVar name Nothing) <$> pIdentImpl
    , parens pType
    ]
 where
  pAnnotatedTVar = do
    name <- pIdentImpl
    _ <- symbol ":"
    k <- pKind
    pure $ TVar name (Just k)

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

-- | Parse a type variable binder: either 'a' or '(a : kind)'
pTyVarBinder :: Parser (String, Kind)
pTyVarBinder =
  choice
    [ parens $ do
        name <- pIdentImpl
        _ <- symbol ":"
        k <- pKind
        pure (name, k)
    , do
        name <- pIdentImpl
        pure (name, KStar)
    ]

-- Forall types: forall a. a -> a
pForAll :: Parser Ty
pForAll = do
  _ <- symbol "forall" <|> symbol "∀"
  binders <- some pTyVarBinder
  _ <- symbol "."
  ty <- pArrowTy
  pure $ foldr (\(name, k) acc -> TForall name k acc) ty binders

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

-- Lambda expression: \x y -> expr
pLambda :: Parser Expr
pLambda = do
  _ <- symbol "\\"
  binders <- some pIdent
  _ <- alternativeSym "->" "→"
  body <- parseExpr
  pure $ ELam binders body

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
    , EUnit <$ symbol "()"
    , pLambda
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
pFunctionParam =
  choice
    [ -- Binder parameter with type annotation: (x: Int)
      try $ do
        name <- pIdent
        _ <- symbol ":"
        ty <- pType
        pure $ BinderParam (Binder name (Just ty))
    , -- Type parameter with kind annotation: {α : ∀α.α}
      try $ do
        name <- pIdent
        _ <- symbol ":"
        kind <- pKind
        pure $ TypeParam name kind
    , -- Binder parameter without type annotation: (x)
      do
        name <- pIdent
        pure $ BinderParam (Binder name Nothing)
    ]

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
        funcName
        funcParams
        returnType
        body

-- Parse an instance signature in a typeclass declaration:
-- eq : a -> a -> Bool
pInstanceSig :: Parser (String, Ty)
pInstanceSig = do
  name <- pIdent
  _ <- symbol ":"
  ty <- pType
  pure (name, ty)

-- Parse a typeclass declaration:
-- class Eq a where
--   eq : a -> a -> Bool
pClassDecl :: Parser Decl
pClassDecl = do
  _ <- symbol "class"
  className <- pIdent
  tyParams <- many pTyVarBinder
  _ <- symbol "where"
  methods <- many pInstanceSig
  pure $
    ClassDecl $
      ClassDeclaration
        className
        tyParams
        methods

-- Parse a typeclass instance declaration:
-- instance Eq : Int where
--   def eq (x: Int) (y: Int): Bool := x == y
pClassInstanceDecl :: Parser Decl
pClassInstanceDecl = do
  _ <- symbol "instance"
  className <- pIdent
  _ <- symbol ":"
  tyArgs <- many pType
  _ <- symbol "where"
  methods <- many pFunctionDecl
  methods' <- mapM extractFunctionDecl methods

  pure $
    ClassInstanceDecl $
      ClassInstance
        className
        tyArgs
        methods'
 where
  extractFunctionDecl :: Decl -> Parser FunctionDeclaration
  extractFunctionDecl (FunctionDecl fd) = pure fd
  extractFunctionDecl _ = fail "Expected a function declaration in class instance"

-- Parse a site path:
-- f: ~a -> ~b
pSitePath :: Parser SitePath
pSitePath = do
  name <- pIdent
  _ <- symbol ":"
  source <- pSiteCover
  _ <- alternativeSym "->" "→"
  target <- pSiteCover
  pure $ SitePath name source target

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
  pure $
    SiteDecl $
      SiteDeclaration
        siteName
        siteCovers
        sitePaths

pSiteCover :: Parser String
pSiteCover = symbol "~" *> pIdent

parseDecl :: Parser Decl
parseDecl =
  choice
    [ try pEnumDecl
    , try pStructDecl
    , try pFunctionDecl
    , try pClassDecl
    , try pImportDecl
    , try pSiteDecl
    ]

parseProgram :: Parser [Decl]
parseProgram = many (parseDecl <* optional (symbol ";"))
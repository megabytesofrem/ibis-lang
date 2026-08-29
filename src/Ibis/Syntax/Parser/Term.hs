{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Syntax.Parser.Term where

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L

import Ibis.Syntax.AST.Surface
import Ibis.Syntax.Parser.Lexer (Parser, lexeme, pIdent, pLiteral, parens, symbol)
import Ibis.Syntax.Parser.Pattern (pPattern)

-- Parse a typed pair: x : A
typedPair :: Parser Param
typedPair = do
  name <- pIdent
  _ <- symbol ":"
  typ <- pTerm
  pure $ Param name typ

telescope :: Parser [Param]
telescope = many typedPair

-- Universe level parsing (e.g Type 0, Type 1, etc.)
pUniverse :: Parser Term
pUniverse = do
  _ <- symbol "Type"
  level <- lexeme L.decimal
  pure $ Universe level

-- Topological site parsing (e.g @SiteName)
pSite :: Parser Term
pSite = do
  name <- symbol "@" *> pIdent
  pure $ Site name

-- Parse a cover term: Cover u v
pCover :: Parser Term
pCover = do
  _ <- symbol "Cover"
  u <- pTerm
  v <- pTerm
  pure $ Cover u v

-- Parse a section term: Sect A u
pSect :: Parser Term
pSect = do
  _ <- symbol "Sect"
  a <- pTerm
  u <- pTerm
  pure $ Sect a u

-- Parse a restriction term: res u to v
pRes :: Parser Term
pRes = do
  _ <- symbol "res"
  sect <- pAtom
  _ <- symbol "to"
  site <- pAtom
  pure $ Res sect site

-- Parse an extension term: ext s @v -> @w
pExt :: Parser Term
pExt = do
  _ <- symbol "ext"
  sect <- pAtom
  srcSite <- pAtom
  _ <- symbol "->"
  destSite <- pAtom
  pure $ Ext sect srcSite destSite

-------------------------------------------------------------

pList :: Parser Term
pList = do
  _ <- symbol "["
  elems <- pTerm `sepBy` symbol ","
  _ <- symbol "]"
  pure $ List elems

-- Atomic terms: leaf nodes or terms explicitly enclosed by matching delimiters
pAtom :: Parser Term
pAtom =
  choice
    [ pUniverse
    , pSite
    , pList
    , parens pTermSeq
    , Var <$> pIdent
    , Lit <$> pLiteral
    ]

-- Application chain: f x y z
pApp :: Parser Term
pApp = do
  headTerm <- pAtom
  args <- many pAtom
  pure $ foldl' App headTerm args

-- Handles parenthesized terms, pairs (a, b), and type annotations (x : T)
pTermSeq :: Parser Term
pTermSeq = do
  t <- pTerm
  mOp <- optional (symbol "," <|> symbol ":")
  case mOp of
    Just "," -> Pair t <$> pTerm
    Just ":" -> Ann t <$> pTerm
    _ -> pure t

-- Dependent function types (Π (x : A) . B)
pPi :: Parser Term
pPi = do
  _ <- symbol "Π" <|> symbol "Pi"
  param <- parens typedPair
  _ <- symbol "."
  body <- pTerm
  pure $ Pi (paramName param) (paramType param) body

-- Dependent product types (Σ (x : A) . B)
pSigma :: Parser Term
pSigma = do
  _ <- symbol "Σ" <|> symbol "Sigma"
  param <- parens typedPair
  _ <- symbol "."
  body <- pTerm
  pure $ Sigma (paramName param) (paramType param) body

pFst :: Parser Term
pFst = do
  _ <- symbol "fst"
  t <- pAtom -- Consumes an atom so `fst x y` parses as `(fst x) y`
  pure $ Fst t

pSnd :: Parser Term
pSnd = do
  _ <- symbol "snd"
  t <- pAtom
  pure $ Snd t

pLet :: Parser Term
pLet = do
  _ <- symbol "let"
  name <- pIdent
  mTyp <- optional (symbol ":" *> pTerm)
  _ <- symbol "="
  value <- pTerm
  _ <- symbol "in"
  body <- pTerm
  pure $ Let name mTyp value body

pIf :: Parser Term
pIf = do
  _ <- symbol "if"
  cond <- pTerm
  _ <- symbol "then"
  thenBranch <- pTerm
  _ <- symbol "else"
  elseBranch <- pTerm
  pure $ If cond thenBranch elseBranch

pMatchArm :: Parser (Pat, Term)
pMatchArm = do
  _ <- symbol "|"
  pat <- pPattern
  _ <- symbol "->"
  body <- pTerm
  pure (pat, body)

pMatch :: Parser Term
pMatch = do
  _ <- symbol "match"
  expr <- pTerm
  _ <- symbol "with"
  arms <- many pMatchArm
  pure $ Match expr arms

-- Top-level term parser hierarchy
pTerm :: Parser Term
pTerm =
  choice
    [ try pPi
    , try pSigma
    , try pLet
    , try pIf
    , try pMatch
    , try pFst
    , try pSnd
    , pApp -- pApp handles atoms and applications
    ]

-------------------------------------------------------------
-- DECLARATION PARSERS
--------------------------------------------------------------

-- Parse a covering rule: cover @Parent = {@Child1, @Child2, ...}
pCoveringRule :: Parser CoverRule
pCoveringRule = do
  _ <- symbol "cover"
  parent <- symbol "@" *> pIdent
  _ <- symbol ":="
  children <-
    between (symbol "{") (symbol "}") $
      (symbol "@" *> pIdent) `sepBy` symbol ","
  pure $ CoverRule parent children

-- Parse a path between two covers: path name : @src -> @dest
pSitePath :: Parser SitePath
pSitePath = do
  _ <- symbol "path"
  name <- pIdent
  _ <- symbol ":="
  src <- symbol "@" *> pIdent
  _ <- symbol "->"
  dest <- symbol "@" *> pIdent
  pure $ SitePath name src dest

-- Parse a site declaration:
-- site SiteName where
--   cover @J := {@Child1, @Child2, ...}
--   cover @K := {@Child3, @Child4, ...}
--   path name := @src -> @dest
pSiteDeclaration :: Parser SiteDeclaration
pSiteDeclaration = do
  _ <- symbol "site"
  name <- pIdent
  _ <- symbol "where"
  covers <- many pCoveringRule
  paths <- many pSitePath
  pure $ SiteDeclaration name covers paths

-- Parse a struct declaration:
-- struct Buffer (site : Site) where
--   capacity : Nat
--   data : Array capacity
pStructDeclaration :: Parser StructDecl
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
    fieldType <- pTerm
    pure (fieldName, fieldType)

-- Parse an inductive constructor:
--   Nil : Vect A 0
--   Cons : (x : A) -> (xs : Vect A n) -> Vect A (n + 1)
pInductiveConstructor :: Parser InductiveCtor
pInductiveConstructor = do
  name <- pIdent
  _ <- symbol ":"
  typ <- pTerm
  pure $ InductiveCtor name typ

-- Parse an inductive declaration:
-- inductive Vect (A : Type 0) : Nat -> Type 0 where
--   Nil : Vect A 0
--   Cons : (x : A) -> (xs : Vect A n) -> Vect A (n + 1)
pInductiveDeclaration :: Parser InductiveDecl
pInductiveDeclaration = do
  _ <- symbol "inductive"
  name <- pIdent
  params <- telescope -- Consumes fixed parameters like (A : Type 0)
  _ <- symbol ":"
  arity <- pTerm -- Consumes index arity + universe like Nat -> Type 0
  _ <- symbol "where"
  ctors <- many pInductiveConstructor
  pure $ InductiveDecl name params arity ctors

-- Top-level declaration parser
pDecl :: Parser Decl
pDecl =
  choice
    [ TermDecl <$> pTerm
    , StructDecl' <$> pStructDeclaration
    , InductiveDecl' <$> pInductiveDeclaration
    , SiteDecl <$> pSiteDeclaration
    ]
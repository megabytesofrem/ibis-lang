{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Syntax.Parser.Term where

import Control.Monad (guard)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L

import Ibis.Syntax.AST.Surface
import Ibis.Syntax.Parser.Lexer (Parser, lexeme, pIdent, pLiteral, parens, symbol)
import Ibis.Syntax.Parser.Pattern (pPattern)
import Ibis.Syntax.Parser.Tactic (pByBlock)

-- Parse a typed pair: x : A
typedPair :: Parser Param
typedPair = do
  name <- pIdent
  _ <- symbol ":"
  typ <- pTerm
  pure $ Param name typ

-- Parse a telescope of typed pairs: (x : A) (y : B) ...
telescope :: Parser [Param]
telescope = many typedPair

-- Universe level parsing (e.g Type 1, etc.)
--
-- Universe 0 cannot be constructed via 'Type 0' to avoid Girard's paradox
pUniverse :: Parser Term
pUniverse = do
  _ <- symbol "Type"
  level <- lexeme L.decimal

  guard (level >= 0) <?> "Universe level must be non-negative"
  guard (level > 0) <?> "Universe cannot be zero; use 'Prop'"

  pure $ Universe level

-- Parse the Prop universe (Prop is Universe 0)
pPropUniverse :: Parser Term
pPropUniverse = do
  _ <- symbol "Prop"
  pure $ Universe 0

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
    , pPropUniverse
    , pSite
    , pList
    , parens pParenTerm
    , Var <$> pIdent
    , Lit <$> pLiteral
    ]

pParenTerm :: Parser Term
pParenTerm =
  try pAnnotated <|> pTupleOrSingle
 where
  -- (x : T)
  pAnnotated = do
    name <- pIdent
    _ <- symbol ":"
    typ <- pTerm
    pure $ Ann (Var name) typ

  -- (a) or (a, b, c)
  pTupleOrSingle = do
    terms <- pTerm `sepBy` symbol ","
    case terms of
      [t] -> pure t
      ts -> pure $ foldr1 Pair ts

-- Application chain: f x y z
pApp :: Parser Term
pApp = do
  headTerm <- pAtom
  args <- many pAtom
  pure $ foldl' App headTerm args

-- Dependent function types ((x : A) -> B)
pPi :: Parser Term
pPi = do
  _ <- optional (symbol "Π" <|> symbol "Pi")
  param <- try $ parens typedPair
  _ <- symbol "->"
  body <- pTerm
  pure $ Pi (paramName param) (paramType param) body

-- Dependent product types ((x : A, B))
pSigma :: Parser Term
pSigma = do
  _ <- optional (symbol "Σ" <|> symbol "Sigma")
  first <- parens typedPair
  _ <- symbol ","
  second <- pTerm
  pure $ Sigma (paramName first) (paramType first) second

-- Function types: A -> B or (pApp) -> pTerm
pFuncType :: Parser Term
pFuncType = do
  dom <- pApp

  -- Check for an optional codomain after the arrow
  mCod <- optional (symbol "->" *> pTerm)
  case mCod of
    Just cod -> pure $ Pi "_" dom cod
    Nothing -> pure dom

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
    , pFuncType
    ]

-------------------------------------------------------------
-- DECLARATION PARSERS
--------------------------------------------------------------

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
pSiteDeclaration :: Parser SiteDeclaration
pSiteDeclaration = do
  _ <- symbol "site"
  name <- pIdent
  _ <- symbol "where"
  covers <- many pCoveringRule
  pure $ SiteDeclaration name covers

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

pFunctionBody :: Parser FunctionBody
pFunctionBody =
  choice
    [ SimpleBody <$> pTerm -- def f (x : A) (y : B) : C := e
    , TacticBody <$> pByBlock -- def f (x : A) (y : B) : C := by ...
    ]

-- Parse a function declaration:
-- def f (x : A) (y : B) : C := e
-- OR
-- def f (x : A) (y : B) : C := by
--   intro z
--   exact (g z)
pFunctionDeclaration :: Parser FunctionDecl
pFunctionDeclaration = do
  _ <- symbol "def"
  name <- pIdent
  params <- telescope
  _ <- symbol ":"
  returnType <- pTerm
  _ <- symbol ":="
  body <- pFunctionBody
  pure $
    FunctionDecl
      name
      params
      returnType
      body

-- Top-level declaration parser
pDecl :: Parser Decl
pDecl =
  choice
    [ TermDecl <$> pTerm
    , StructDecl' <$> pStructDeclaration
    , InductiveDecl' <$> pInductiveDeclaration
    , FunctionDecl' <$> pFunctionDeclaration
    , SiteDecl <$> pSiteDeclaration
    ]
{-# LANGUAGE ImportQualifiedPost #-}

module Ibis.Syntax.Parser.Term where

import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L

import Ibis.Syntax.AST.Operator (Binop (..), Unop (..))
import Ibis.Syntax.AST.Surface
import Ibis.Syntax.Parser.Lexer (Parser, lexeme, pCtorName, pIdent, pLiteral, parens, symbol)
import Ibis.Syntax.Parser.Pattern (pPattern)
import Ibis.Syntax.Parser.Tactic (pByBlock)

-- Parse a typed pair: x : A
typedPair :: Parser Param
typedPair = do
  name <- pIdent
  _ <- symbol ":"
  typ <- pExpr
  pure $ Param name typ

arrow :: Parser String
arrow = symbol "->" <|> symbol "→"

-- Parse a telescope of typed pairs: (x : A) (y : B) ...
telescope :: Parser [Param]
telescope = many typedPair

-- Universe level parsing (e.g Type 1, etc.)
--
-- Universe 0 cannot be constructed via 'Type 0' to avoid Girard's paradox
pNumericUniverse :: Parser SurfaceUniverse
pNumericUniverse = do
  level <- lexeme L.decimal

  guard (level >= 0) <?> "Universe level must be non-negative"
  guard (level > 0) <?> "Universe cannot be zero; use 'Prop'"

  pure $ UnivLevel level

pNamedUniverse :: Parser SurfaceUniverse
pNamedUniverse = do
  name <- pIdent
  pure $ UnivName name

pUniverse :: Parser Term
pUniverse = do
  _ <- symbol "Type"
  univ <- pNumericUniverse <|> pNamedUniverse
  pure $ Universe univ

-- Parse the Prop universe (Prop is Universe 0)
pPropUniverse :: Parser Term
pPropUniverse = do
  _ <- symbol "Prop"
  pure $ Universe (UnivLevel 0)

-- Topological site parsing (e.g @SiteName)
pSite :: Parser Term
pSite = do
  name <- symbol "@" *> pIdent
  pure $ Site name

-- Parse a cover term: Cover u v
pCover :: Parser Term
pCover = do
  _ <- symbol "Cover"
  u <- pExpr
  v <- pExpr
  pure $ Cover u v

-- Parse a section term: Sect A u
pSect :: Parser Term
pSect = do
  _ <- symbol "Sect"
  a <- pExpr
  u <- pExpr
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
  _ <- arrow
  destSite <- pAtom
  pure $ Ext sect srcSite destSite

-------------------------------------------------------------

pList :: Parser Term
pList = do
  _ <- symbol "["
  elems <- pExpr `sepBy` symbol ","
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
    , Const <$> pCtorName
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
    typ <- pExpr
    pure $ Ann (Var name) typ

  -- (a) or (a, b, c)
  pTupleOrSingle = do
    terms <- pExpr `sepBy` symbol ","
    case terms of
      [t] -> pure t
      ts -> pure $ foldr1 Pair ts

-- Application chain: f x y z
pApp :: Parser Term
pApp = do
  headTerm <- pAtom
  args <- many pAtom
  pure $ foldl' App headTerm args

-- Expression parser with operator precedence from 'operatorTable'.
pExpr :: Parser Term
pExpr = makeExprParser pExprTerm operatorTable

-- Top-level term parser hierarchy
pExprTerm :: Parser Term
pExprTerm =
  choice
    [ try pCover
    , try pSect
    , try pRes
    , try pExt
    , try pFst
    , try pSnd
    , try pPi
    , try pSigma
    , try pLet
    , try pIf
    , try pMatch
    , try pDoNotation
    , pFuncType
    ]

-- Dependent function types ((x : A) -> B)
pPi :: Parser Term
pPi = do
  _ <- optional (symbol "Π" <|> symbol "/Pi")
  param <- try $ parens typedPair
  _ <- arrow
  body <- pExpr
  pure $ Pi (paramName param) (paramType param) body

-- Dependent product types ((x : A, B))
pSigma :: Parser Term
pSigma = do
  _ <- optional (symbol "Σ" <|> symbol "/Sigma")
  first <- parens typedPair
  _ <- symbol ","
  second <- pExpr
  pure $ Sigma (paramName first) (paramType first) second

-- Function types: A -> B or (pApp) -> pExpr
pFuncType :: Parser Term
pFuncType = do
  dom <- pApp

  -- Check for an optional codomain after the arrow
  mCod <- optional (arrow *> pExpr)
  case mCod of
    Just cod -> pure $ Pi "_" dom cod
    Nothing -> pure dom

pFst :: Parser Term
pFst = do
  _ <- symbol "fst"
  t <- pAtom -- Consumes one atom so `fst x y` parses as `(fst x) y`
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
  mTyp <- optional (symbol ":" *> pExpr)
  _ <- symbol "="
  value <- pExpr
  _ <- symbol "in"
  body <- pExpr
  pure $ Let name mTyp value body

pIf :: Parser Term
pIf = do
  _ <- symbol "if"
  cond <- pExpr
  _ <- symbol "then"
  thenBranch <- pExpr
  _ <- symbol "else"
  elseBranch <- pExpr
  pure $ If cond thenBranch elseBranch

pMatchArm :: Parser (Pat, Term)
pMatchArm = do
  _ <- symbol "|"
  pat <- pPattern
  _ <- arrow
  body <- pExpr
  pure (pat, body)

pMatch :: Parser Term
pMatch = do
  _ <- symbol "match"
  expr <- pExpr
  _ <- symbol "with"
  arms <- many pMatchArm
  pure $ Match expr arms

-------------------------------------------------------------
-- DECLARATION PARSERS
--------------------------------------------------------------

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
pFunctionBody =
  choice
    [ SimpleBody <$> pExpr -- def f (x : A) (y : B) : C := e
    , TacticBody <$> pByBlock -- def f (x : A) (y : B) : C := by ...
    ]

-- Parse a function declaration:
-- def f (x : A) (y : B) : C := e
-- OR
-- def f (x : A) (y : B) : C := by
--   intro z
--   exact (g z)
pFunctionDeclaration :: Parser Decl
pFunctionDeclaration = do
  _ <- symbol "def"
  name <- pIdent
  params <- telescope
  _ <- symbol ":"
  returnType <- pExpr
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
    [ TermDecl <$> pExpr
    , pStructDeclaration
    , pInductiveDeclaration
    , pFunctionDeclaration
    , pSiteDeclaration
    ]

-------------------------------------------------------------

-- Operator precedence and associativity for parsing expressions
operatorTable :: [[Operator Parser Term]]
operatorTable =
  [ [prefix "-" (Unop OpNegate)]
  , -- Compose has the highest precedence, so that f . g x parses as (f . g) x
    [infixR "." (Binop OpCompose)]
  ,
    [ infixL "*" (Binop OpMul)
    , -- \* and / have higher precedence than + and -
      infixL "/" (Binop OpDiv)
    ]
  ,
    [ infixL "+" (Binop OpAdd)
    , infixL "-" (Binop OpSub)
    ]
  ,
    [ infixN "<=" (Binop OpLeq)
    , infixN ">=" (Binop OpGeq)
    , infixN "<" (Binop OpLt)
    , infixN ">" (Binop OpGt)
    , infixN "==" (Binop OpEq)
    , infixN "~=" (Binop OpIso)
    , infixN "!=" (Binop OpNeq)
    ]
  ,
    [ infixL "<$>" (Binop OpMap)
    , infixL "<*>" (Binop OpApp)
    ]
  , [infixL "and" (Binop OpAnd)]
  , [infixL "or" (Binop OpOr)]
  , [infixR "==>" (Binop OpImply)]
  , [infixL ">>=" (Binop OpBind)]
  ]
 where
  prefix :: String -> (Term -> Term) -> Operator Parser Term
  prefix name f = Prefix (f <$ symbol name)

  -- Left-associative infix operator (e.g. addition, multiplication)
  infixL :: String -> (Term -> Term -> Term) -> Operator Parser Term
  infixL name f = InfixL (f <$ symbol name)

  -- Right-associative infix operator (e.g. function composition)
  infixR :: String -> (Term -> Term -> Term) -> Operator Parser Term
  infixR name f = InfixR (f <$ symbol name)

  -- Non-associative infix operator (e.g. comparison operators)
  infixN :: String -> (Term -> Term -> Term) -> Operator Parser Term
  infixN name f = InfixN (f <$ symbol name)

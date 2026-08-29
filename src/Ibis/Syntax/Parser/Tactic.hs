module Ibis.Syntax.Parser.Tactic where

import Text.Megaparsec

import Ibis.Syntax.AST.Surface (Tactic (..))
import Ibis.Syntax.Parser.Lexer (Parser, pIdent, symbol)
import {-# SOURCE #-} Ibis.Syntax.Parser.Term (pApp)

pTacticIntro :: Parser Tactic
pTacticIntro = TacticIntro <$> (symbol "intro" *> pIdent)

pTacticExact :: Parser Tactic
pTacticExact = TacticExact <$> (symbol "exact" *> pApp)

pTacticApply :: Parser Tactic
pTacticApply = TacticApply <$> (symbol "apply" *> pApp)

pTacticRfl :: Parser Tactic
pTacticRfl = TacticRfl <$ symbol "rfl"

pTacticSimp :: Parser Tactic
pTacticSimp = TacticSimp <$> (symbol "simp" *> pApp)

pTacticCases :: Parser Tactic
pTacticCases = TacticCases <$> (symbol "cases" *> pApp)

pTacticInduction :: Parser Tactic
pTacticInduction = TacticInduction <$> (symbol "induction" *> pApp)

pTacticBind :: Parser Tactic
pTacticBind = do
  _ <- symbol "bind"
  name <- pIdent
  mTyp <- optional (symbol ":" *> pApp)
  _ <- symbol "="
  value <- pApp
  pure $ TacticBind name mTyp value

pTacticHave :: Parser Tactic
pTacticHave = do
  _ <- symbol "have"
  name <- pIdent
  mTyp <- optional (symbol ":" *> pApp)
  _ <- symbol "="
  value <- pApp
  pure $ TacticHave name mTyp value

pTacticShow :: Parser Tactic
pTacticShow = TacticShow <$> (symbol "show" *> pApp)

pTacticSorry :: Parser Tactic
pTacticSorry = TacticSorry <$ symbol "sorry"

pTacticPathAcross :: Parser Tactic
pTacticPathAcross = do
  _ <- symbol "path_across"
  u <- pApp
  v <- pApp
  pure $ TacticPathAcross u v

pTacticCovers :: Parser Tactic
pTacticCovers = do
  _ <- symbol "covers"
  u <- pApp
  v <- pApp
  pure $ TacticCovers u v

pTacticRes :: Parser Tactic
pTacticRes = do
  _ <- symbol "res"
  u <- pApp
  v <- pApp
  pure $ TacticRes u v

pTacticLan :: Parser Tactic
pTacticLan = do
  _ <- symbol "lan"
  u <- pApp
  v <- pApp
  pure $ TacticLan u v

pTacticGlue :: Parser Tactic
pTacticGlue = do
  _ <- symbol "glue"
  u <- pApp
  v <- pApp
  secU <- pApp
  secV <- pApp
  pure $ TacticGlue u v secU secV

pTactic :: Parser Tactic
pTactic =
  choice
    [ try pTacticIntro -- intro x
    , try pTacticExact -- exact e
    , try pTacticApply -- apply e
    , try pTacticRfl -- rfl
    , try pTacticSimp -- simp e
    , try pTacticCases -- cases e
    , try pTacticInduction -- induction e
    , try pTacticBind -- bind x : A = e
    , try pTacticHave -- have x : A = e
    , try pTacticShow -- show e
    , try pTacticSorry -- sorry
    , try pTacticPathAcross -- path_across u v
    , try pTacticCovers -- covers u v
    , try pTacticRes -- res u v
    , try pTacticLan -- lan u v
    , try pTacticGlue -- glue u v secU secV
    ]

pByBlock :: Parser [Tactic]
pByBlock = do
  _ <- symbol "by"
  -- Parse a sequence of tactics seperated by optional semicolons
  tactics <- many (pTactic <* optional (symbol ";"))
  pure tactics
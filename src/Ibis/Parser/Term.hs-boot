-- src/Ibis/Syntax/Parser/Term.hs-boot
module Ibis.Parser.Term where

import Ibis.AST.Surface (Term)
import Ibis.Parser.Lexer (Parser)

pApp :: Parser Term
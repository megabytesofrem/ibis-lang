-- src/Ibis/Syntax/Parser/Term.hs-boot
module Ibis.Syntax.Parser.Term where

import Ibis.Syntax.AST.Surface (Term)
import Ibis.Syntax.Parser.Lexer (Parser)

pApp :: Parser Term
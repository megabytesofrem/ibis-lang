module Ibis.Parser.Decl where

import Ibis.AST.Surface (Decl, ForwardDecl, Term)
import Ibis.Parser.Lexer (Parser)

pDoNotation :: Parser Term
pForwardDeclaration :: Parser ForwardDecl
pFunctionDeclaration :: Parser Decl
pSiteDeclaration :: Parser Decl
pStructDeclaration :: Parser Decl
pInductiveDeclaration :: Parser Decl

-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.File (
  PFileContent (..), fileP
) where

import Elyon.Parser (Parser, lxs)
import Elyon.Parser.Terms (PTerm, PTPattern, termP, termPatternP)
import Elyon.Parser.Assignments (PAssignment, assignmentP)
import Elyon.Parser.Traits (PTraitDecl, traitP, implP) 
import Elyon.Parser.UseStatements (PUseStatement, useStmtP)
import Elyon.Parser.Dat (PDef, datP, defP)
import Text.Parsec.Indent (topLevel)
import Text.Parsec (many, char, (<|>), string, eof)

data PFileContent =
    PFA_Assignment (PAssignment PTPattern PTerm)
  | PFA_TraitDecl PTraitDecl
  | PFA_ImplDecl PTraitDecl
  | PFA_UseStmt PUseStatement
  | PFA_Dat PTPattern
  | PFA_Def PDef
  deriving (Show, Eq)

fileP :: Parser [PFileContent]
fileP = lxs (pure ()) 
  *> many (topLevel *> lxs fileContentP)
  <* lxs (pure ()) <* eof

fileContentP :: Parser PFileContent
fileContentP =
      fmap PFA_TraitDecl traitP
  <|> fmap PFA_ImplDecl implP
  <|> fmap PFA_UseStmt (string "use " *> useStmtP)
  <|> fmap PFA_Dat datP
  <|> fmap PFA_Def defP
  <|> fmap PFA_Assignment 
      (assignmentP termPatternP termP (char '='))
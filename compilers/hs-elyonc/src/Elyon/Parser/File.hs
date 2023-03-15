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
import Text.Parsec.Indent (topLevel)
import Text.Parsec (many, char)

data PFileContent = PFA_Assignment (PAssignment PTPattern PTerm)
  deriving (Show, Eq)

fileP :: Parser [PFileContent]
fileP = lxs (pure ()) *> many (topLevel *> lxs fileContentP)

fileContentP :: Parser PFileContent
fileContentP = fmap PFA_Assignment 
  (assignmentP termPatternP termP (char '='))
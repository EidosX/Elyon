-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Traits (
  PTraitDecl (..), traitP, implP
) where

import Elyon.Parser (Parser, lx, lxs)
import Elyon.Parser.Terms (PTerm, PTPattern, termP, termPatternP)
import Elyon.Parser.Terms.Simple (varP)
import Elyon.Parser.Assignments (PAssignment, assignmentP)
import Data.Text (Text)
import Text.Parsec (string, char, try)
import Text.Parsec.Indent (indented, withPos, block)

data PTraitDecl = P_TraitDecl {
  td_traitName :: Text,
  td_forPattern :: PTPattern,
  td_assignments :: [PAssignment PTPattern PTerm]
} deriving (Eq, Show)

genericTraitP :: String -> Parser PTraitDecl
genericTraitP firstKeyword = withPos $ do
  _ <- lx (try $ string firstKeyword)
  traitName <- lx (varP <* char ' ')
  _ <- lx (string "for ")
  forPattern <- lxs termPatternP
  assignments <- indented *> 
    block (lxs $ assignmentP termPatternP termP (char '='))
  return $ P_TraitDecl {
    td_traitName = traitName,
    td_forPattern = forPattern,
    td_assignments = assignments
  }

traitP :: Parser PTraitDecl
traitP = genericTraitP "trait"

implP :: Parser PTraitDecl
implP = genericTraitP "impl"
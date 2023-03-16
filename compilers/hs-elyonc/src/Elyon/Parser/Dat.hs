-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Dat (
  PDefContent (..), PDef (..), defP, datP
) where

import Elyon.Parser (Parser, lx, lxs)
import Elyon.Parser.Terms (PTPattern, termPatternP)
import Text.Parsec (string, (<|>), try)
import Text.Parsec.Indent (block, indented)

data PDefContent = PDC_Pattern PTPattern
                 | PDC_Dat PTPattern
  deriving (Show, Eq)

data PDef = PDef {
  pd_pattern :: PTPattern,
  pd_content :: [PDefContent]
} deriving (Show, Eq)

defP :: Parser PDef
defP = do
  patt <- lxs (try (lx $ string "def ") *> termPatternP)
  content <- indented *> block (lxs defContentP)
  return $ PDef { pd_pattern = patt, pd_content = content }

defContentP :: Parser PDefContent
defContentP = fmap PDC_Dat datP 
          <|> fmap PDC_Pattern termPatternP

datP :: Parser PTPattern
datP = (try . lx $ string "dat ") *> termPatternP
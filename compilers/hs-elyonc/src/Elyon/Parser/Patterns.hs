-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Patterns (
  PPattern (..), patternP
) where

import Text.Parsec ((<|>), char, try)
import Data.Functor (($>))
import Elyon.Parser (Parser, lx)

data PPattern p c = 
    PP_Match p
  | PP_Wildcard
  | PP_Cond (PPattern p c) c
  deriving (Eq)

simplePatternP :: forall c p. Parser p -> Parser (PPattern p c)
simplePatternP termPattP = (char '_' $> PP_Wildcard)
                       <|> fmap PP_Match termPattP

patternP :: Parser p -> Parser c -> Parser (PPattern p c)
patternP termPattP termCondP = do
  simple <- simplePatternP termPattP
  let condP = try (lx (pure ()) *> lx (char ':')) *> termCondP
  fmap (PP_Cond simple) condP <|> return simple


instance (Show p, Show c) => Show (PPattern p c) where
  show (PP_Match t) = show t
  show (PP_Wildcard) = "_"
  show (PP_Cond p t) = show p <> ": " <> show t

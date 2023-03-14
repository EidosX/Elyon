-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Primitives (
  intP, floatP, charP, interpolatedStringP,
  InterpolatedStringContent (..)
) where

import Elyon.Parser (Parser)
import Data.Text (Text)
import Text.Parsec (many1, digit, char, between, 
  noneOf, many, (<|>))
import Data.String (fromString)
import Control.Applicative (liftA2)
import Data.Functor (($>))

data InterpolatedStringContent e = ISCInterpolated e
                                 | ISCString Text
  deriving (Eq, Show)

intP :: Parser Text
intP = fromString <$> many1 digit

floatP :: Parser (Text, Text)
floatP = liftA2 (,) (intP <* char '.') intP

-- Parses e.g n or \n
escapedCharP :: [Char] -> Parser Char
escapedCharP forbidden = (char '\\' *> escaped) <|> noneOf forbidden
  where escaped = (char 'a' $> '\a')
              <|> (char 'b' $> '\b')
              <|> (char 'f' $> '\f')
              <|> (char 'n' $> '\n')
              <|> (char 'r' $> '\r')
              <|> (char 't' $> '\t')
              <|> (char 'v' $> '\v')
              <|> (char '\\' $> '\\')
              <|> (char '\'' $> '\'')
              <|> (char '\"' $> '\"')

charP :: Parser Char
charP = between (char '\'') (char '\'') $
  escapedCharP ['\'']

interpolatedStringP :: Parser e -> Parser [InterpolatedStringContent e]
interpolatedStringP p = between (char '"') (char '"') $
  many (normal <|> interpolated)
  where normal = fmap (ISCString . fromString) $
          many1 (escapedCharP "\"{")
        interpolated = fmap (ISCInterpolated) $
          between (char '{') (char '}') p
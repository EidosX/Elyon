-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Primitives (
  intP, floatP, charP, templateStringP,
  TemplateStringContent (..), Sign (..)
) where

import Elyon.Parser (Parser, lx)
import Data.Text (Text)
import Text.Parsec (many1, digit, char, between, 
  noneOf, many, try, (<|>))
import Data.String (fromString)
import Data.Functor (($>))

data TemplateStringContent e = ISCInterpolated e
                             | ISCString Text
  deriving (Eq, Show)

data Sign = Plus | Minus
  deriving (Eq, Show)

natP :: Parser Text
natP = fromString <$> many1 digit

intP :: Parser (Sign, Text)
intP = fmap (Plus,) natP <|> try (char '-' *> fmap (Minus,) natP)

floatP :: Parser (Sign, Text, Text)
floatP = do
  (s, l) <- intP
  _ <- char '.'
  r <- natP
  return (s, l, r)

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

templateStringP :: Parser e -> Parser [TemplateStringContent e]
templateStringP p = between (char '"') (char '"') $
  many (normal <|> interpolated)
  where normal = fmap (ISCString . fromString) $
          many1 (escapedCharP "\"{")
        interpolated = fmap ISCInterpolated $
          between (lx $ char '{') (char '}') (lx p)
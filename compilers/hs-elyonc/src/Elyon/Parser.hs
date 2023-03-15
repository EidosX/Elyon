-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser (
  Parser, quickParse, lx, lxs
) where

import Text.Parsec.Indent (IndentParser, runIndentParser)
import Text.Parsec (many, space, endOfLine, try, string,
  (<|>), manyTill, anyChar, ParseError)
import Data.Text (Text)
import Data.Functor (($>))

type Parser a = IndentParser Text () a

quickParse :: Parser a -> Text -> Either ParseError a
quickParse p = runIndentParser p () "quickparse.ely"

lx :: Parser a -> Parser a
lx p = p <* many space

lxs :: Parser a -> Parser a
lxs p = p <* many (space <|> endOfLine <|> (commentP $> ' '))

commentP :: Parser String
commentP = try (string "//") *>
  manyTill anyChar (try endOfLine)
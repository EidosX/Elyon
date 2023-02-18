-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Parser (
  Parser, quickParse, termP
) where

import Text.Parsec.Indent (IndentParser, runIndentParser)
import Text.Parsec (many, many1, anyChar, between, string, 
  sepEndBy, digit, noneOf, try, (<|>))
import Text.Parsec.Error (ParseError)
import Control.Applicative (liftA2)
import Lib (Term (..), intToTerm, floatToTerm, charToTerm,
            listToTerm, stringToTerm)

type Parser a = IndentParser String () a

quickParse :: Parser a -> String -> Either ParseError a
quickParse p = runIndentParser p () "quickparse.ely"

termP :: Parser Term
termP = (try (fmap floatToTerm floatP) <|> fmap intToTerm intP)
    <|> fmap charToTerm charP
    <|> fmap listToTerm (listP termP)
    <|> fmap stringToTerm stringP

intP :: Parser String
intP = many1 digit

floatP :: Parser (String, String)
floatP = liftA2 (,) (intP <* string ".") intP

charP :: Parser Char
charP = between (string "'") (string "'") anyChar

listP :: Parser a -> Parser [a]
listP p = between (string "[") (string "]") $
  sepEndBy p (string ",")

stringP :: Parser String
stringP = between (string "\"") (string "\"") (many (noneOf "\""))
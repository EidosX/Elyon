-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Parser (
  Parser, quickParse, termP
) where

import Text.Parsec.Indent (IndentParser, runIndentParser)
import Text.Parsec (many, many1, anyChar, between, string, 
  sepEndBy, sepEndBy1, digit, noneOf, try, upper, lower,
  oneOf, satisfy, (<|>))
import Text.Parsec.Error (ParseError)
import Control.Applicative (liftA2)
import Lib (Pattern (..), Term (..), intToTerm, floatToTerm, charToTerm,
            listToTerm, stringToTerm)
import Data.Char (isSymbol)

type Parser a = IndentParser String () a

quickParse :: Parser a -> String -> Either ParseError a
quickParse p = runIndentParser p () "quickparse.ely"

singleTermP :: Parser Term
singleTermP = do
  firstTerm <- singleTermNoApplP
  args <- fmap concat (many1 (argsP termP)) <|> return []
  return $ foldl TermAppl firstTerm args

singleTermNoApplP :: Parser Term
singleTermNoApplP = (try (fmap floatToTerm floatP) <|> fmap intToTerm intP)
          <|> fmap charToTerm charP
          <|> fmap listToTerm (listP termP)
          <|> fmap stringToTerm stringP
          <|> fmap TermVar varP
          <|> parens termP

termP :: Parser Term
termP = try lambdaP <|> singleTermP
operatorP :: Parser String
operatorP = many1 (satisfy isSymbol <|> oneOf "<>+-/*$%@€£?&§")

patternP :: Parser Pattern
patternP = fmap PatternMatch singleTermP

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

varP :: Parser String
varP = many1 (oneOf "_" <|> upper <|> lower)

lambdaP :: Parser Term
lambdaP = do
  args <- argsP patternP
  _ <- string "->"
  t <- termP
  return $ foldr TermLambda t args

argsP :: Parser a -> Parser [a]
argsP p = between (string "(") (string ")") $ sepEndBy1 p (string ",")

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Parser (
  Parser, quickParse, singleTermP, termP, lx, lxs
) where

import Text.Parsec.Indent (IndentParser, runIndentParser)
import Text.Parsec (many, many1, anyChar, between, string, 
  sepEndBy, sepEndBy1, digit, noneOf, try, upper, lower,
  oneOf, satisfy, space, endOfLine, (<|>))
import Text.Parsec.Error (ParseError)
import Control.Applicative (liftA2)
import Lib (Pattern (..), Term (..), (<.<), intToTerm, floatToTerm, charToTerm,
            listToTerm, stringToTerm)
import Data.Char (isSymbol)

type Parser a = IndentParser String () a

quickParse :: Parser a -> String -> Either ParseError a
quickParse p = runIndentParser p () "quickparse.ely"

lx :: Parser a -> Parser a
lx p = p <* many space

lxs :: Parser a -> Parser a
lxs p = p <* many (space <|> endOfLine)

singleTermP :: Parser Term
singleTermP = do
  firstTerm <- lx singleTermNoApplP
  args <- (fmap concat . many1 $ argsP termP) <|> return []
  return $ foldl TermAppl firstTerm args

singleTermNoApplP :: Parser Term
singleTermNoApplP = parens termP
          <|> (try (fmap floatToTerm floatP) <|> fmap intToTerm intP)
          <|> fmap charToTerm charP
          <|> fmap listToTerm (listP termP)
          <|> fmap stringToTerm stringP
          <|> fmap TermVar varP
          <|> fmap TermVar operatorP

termP :: Parser Term
termP = try lambdaP 
    <|> try binopP
    <|> singleTermP

binopP :: Parser Term
binopP = do
  l <- lx singleTermP
  op <- lx operatorP
  r <- singleTermP
  return $ TermVar op <.< l <.< r

operatorP :: Parser String
operatorP = many1 (satisfy isSymbol <|> oneOf "<>+-/*$%@€£?&§:=~")

singlePatternP :: Parser Pattern
singlePatternP = try (parens patternP) 
  <|> fmap PatternMatch singleTermP

patternP :: Parser Pattern
patternP = do
  patt <- singlePatternP
  fmap (PatternCondition patt) (try (many space *> lx (string ":")) *> termP)
    <|> return patt  

intP :: Parser String
intP = many1 digit

floatP :: Parser (String, String)
floatP = liftA2 (,) (intP <* string ".") intP

charP :: Parser Char
charP = between (string "'") (string "'") anyChar

listP :: Parser a -> Parser [a]
listP p = between (lx $ string "[") (string "]") $
  sepEndBy (lx p) (lx $ string ",")

stringP :: Parser String
stringP = between (string "\"") (string "\"") (many (noneOf "\""))

varP :: Parser String
varP = liftA2 (:)
  (upper <|> lower)
  (many (oneOf "_" <|> upper <|> lower <|> digit))

lambdaP :: Parser Term
lambdaP = do
  _ <- lx $ string "fn"
  args <- lx $ argsP patternP
  _ <- lx $ string "->"
  t <- termP
  return $ foldr TermLambda t args

argsP :: Parser a -> Parser [a]
argsP p = parens $ sepEndBy1 (lx p) (lx $ string ",")

parens :: Parser a -> Parser a
parens = between (lx $ string "(") (string ")")

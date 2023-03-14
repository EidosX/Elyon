-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Terms (
  varP, operatorP, 
  patternP,
  termP
) where

import Text.Parsec (many1, many, letter, digit, oneOf, 
  satisfy, try, char, sepEndBy, between, string, (<|>))
import Text.Parsec.Indent (indented)
import Data.Char (isSymbol)
import Data.Functor (($>))
import Control.Applicative (liftA2)
import Data.Text (Text, unpack)
import Data.String (fromString)
import Data.List (intercalate)
import Elyon.Parser (Parser, lx, lxs)
import Elyon.Parser.Primitives (InterpolatedStringContent(..),
  Sign(..), intP, floatP, charP, interpolatedStringP)
import Elyon.Parser.Lists (ListLiteralContent (..), listLiteralP)

data ParserTerm = 
    PTVar Text
  | PTInt (Sign, Text)
  | PTFloat (Sign, Text, Text)
  | PTChar Char
  | PTString [InterpolatedStringContent ParserTerm]
  | PTList [ListLiteralContent ParserTerm]
  | PTAppl ParserTerm [ParserTerm]
  | PTBinops [ParserTerm] -- [3, >, x, >=, 12]
  | PTPartialBinop (Maybe ParserTerm) ParserTerm (Maybe ParserTerm)
  | PTLambda [ParserPattern] ParserTerm
  | PTParens ParserTerm
  deriving (Eq)

instance Show ParserTerm where
  show (PTVar x) = unpack x
  show (PTInt (Plus, x)) = unpack x
  show (PTInt (Minus, x)) = "-" <> unpack x
  show (PTFloat (Plus, l, r)) = unpack l <> "." <> unpack r
  show (PTFloat (Minus, l, r)) = "-" <> unpack l <> "." <> unpack r
  show (PTChar c) = "'" <> [c] <> "'"
  show (PTString s) = "\"" <> foldr f "" s <> "\""
    where f (ISCString s') curr = curr <> unpack s' 
          f (ISCInterpolated t) curr =
              curr <> "{" <> show t <> "}"
  show (PTList l) = show l
  show (PTAppl f args) = show f <> "(" <> 
    intercalate ", " (map show args) <> ")" 
  show (PTBinops binops) = unwords $ map show binops
  show (PTPartialBinop l op r) =
    f l <> " " <> show op <> " " <> f r
    where f Nothing = "_"
          f (Just x) = show x
  show (PTLambda args t) = 
    "fn(" <> intercalate ", " (map show args) <> ") -> " 
    <> show t
  show (PTParens t) = "(" <> show t <> ")"


termP :: Parser ParserTerm
termP = do
  let termNoApplP = 
            lambdaP
        <|> try (fmap PTFloat floatP)
        <|> fmap PTInt intP
        <|> fmap PTChar charP
        <|> fmap PTString (interpolatedStringP termP)
        <|> fmap PTList (listLiteralP termP)
        <|> fmap PTVar varP
        <|> fmap PTVar operatorP
        <|> partialLeftBinopP
        <|> between (char '(') (char ')') (fmap PTParens termP)

  let applsP = lx (return ()) *> many (lx applP)
      applP = between (lx $ char '(') (char ')') $
        sepEndBy (lx termP) (lx $ char ',')
  
  let termWithApplsP = liftA2 (foldl PTAppl) 
        termNoApplP applsP
  
  lTerm <- termWithApplsP

  let partialBinopP = do
        lx (return ()) 
        op <- lx (fmap PTVar operatorP) 
        _ <- char '_'
        return $ PTPartialBinop (Just lTerm) op Nothing
  
  let binopsP = fmap (PTBinops . (lTerm :) . concat) (many1 binopP)
      binopP = do
        lxs (return ())
        op <- indented *> lxs (fmap PTVar operatorP)
        rTerm <- indented *> termWithApplsP
        return [op, rTerm]
  
  try partialBinopP <|> binopsP <|> return lTerm

varP :: Parser Text
varP = fromString <$> 
  liftA2 (:) letter (many (letter <|> digit <|> oneOf "_"))

operatorP :: Parser Text
operatorP = fromString <$> 
  many1 (satisfy isSymbol <|> oneOf "+-=<>?*$§&@#%")

partialLeftBinopP :: Parser ParserTerm
partialLeftBinopP = do
  _ <- lx $ char '_'
  op <- lx (fmap PTVar operatorP)
  r <- (char '_' $> Nothing) <|> fmap Just termP
  return $ PTPartialBinop Nothing op r

lambdaP :: Parser ParserTerm
lambdaP = do
  _ <- lx (string "fn")
  args <- lx (between (lx $ char '(') (char ')') (many1 patternP))
  _ <- lxs (string "->")
  t <- indented *> termP
  return $ PTLambda args t


data ParserPattern = 
    PPMatch ParserTerm
  | PPWildcard
  | PPCond ParserPattern ParserTerm
  deriving (Eq)

instance Show ParserPattern where
  show (PPMatch t) = show t
  show (PPWildcard) = "_"
  show (PPCond p t) = show p <> ": " <> show t

patternP :: Parser ParserPattern
patternP = do
  left <- (char '_' $> PPWildcard) <|> (fmap PPMatch termP)
  let cond = lx (return ()) *> lx (char ':') *> termP
  try (fmap (PPCond left) cond) <|> return left

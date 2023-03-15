-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Terms (
  PTerm (..), PTPattern (..),
  termP, termPatternP
) where

import Elyon.Parser (Parser, lx, lxs)
import Elyon.Parser.IfElse (PIfElse, ifElseP)
import Elyon.Parser.Patterns (PPattern (..), patternP)
import Elyon.Parser.Terms.Simple (simpleTermP, PSimpleTerm (..),
  operatorP, argListP)
import Elyon.Parser.Terms.DoNotation (DoNotationBody (..), 
  doNotationP)
import Data.Functor (($>))
import Data.List (intercalate)
import Control.Applicative (liftA2)
import Text.Parsec ((<|>), try, char, string, many1, between)
import Text.Parsec.Indent (indented, withPos, withBlock)

data PTerm = 
    PT_Simple (PSimpleTerm PTerm)
  | PT_Match PTerm [(PTPattern, PTerm)]
  | PT_IfElse [PIfElse PTerm PTerm] -- Condition Then Else
  | PT_DoNotation (DoNotationBody PTPattern PTerm)
  | PT_Lambda [PTPattern] PTerm
  | PT_Binops [PSimpleTerm PTerm] -- 3 > x >= 12 becomes [3, >, x, >=, 12]
  | PT_PartialBinop
      (Maybe (PSimpleTerm PTerm))
      (PSimpleTerm PTerm)
      (Maybe (PSimpleTerm PTerm))
  deriving (Eq)

newtype PTPattern =
  PTPattern (PPattern (PSimpleTerm PTPattern) PTerm)
  deriving (Eq)

termPatternP :: Parser PTPattern
termPatternP = fmap PTPattern $
  patternP (simpleTermP termPatternP) termP

termP :: Parser PTerm
termP = lambdaP
    <|> matchExprP
    <|> fmap PT_IfElse (ifElseP termP termP)
    <|> fmap PT_DoNotation (doNotationP termPatternP termP)
    <|> partialLeftBinopP
    <|> manyBinopsP
  

matchExprP :: Parser PTerm
matchExprP = withBlock PT_Match (lxs matchTermP) casesP
  where matchTermP =
          try (lx (string "match")) *>
               between (lx $ char '(') (char ')') termP
        casesP = withPos $ lxs (liftA2 (,) 
                  (lx termPatternP <* lxs (string "->")) 
                  (indented *> termP))

lambdaP :: Parser PTerm
lambdaP = try (string "fn") *> do
  args <- lxs $ argListP (char '(') (char ')') termPatternP
  _ <- indented *> lxs (string "->")
  t <- indented *> termP
  return $ PT_Lambda args t

partialLeftBinopP :: Parser PTerm
partialLeftBinopP = lx (char '_') *> do
  op <- fmap PS_Var (lx operatorP)
  r <- (char '_' $> Nothing) <|> fmap Just (simpleTermP termP)
  return $ PT_PartialBinop Nothing op r

manyBinopsP :: Parser PTerm
manyBinopsP = do
  t1 <- simpleTermP termP

  let partialRightBinopP = lx (pure ()) *> do
        op <- fmap PS_Var (lx operatorP)
        _ <- char '_'
        return $ PT_PartialBinop (Just t1) op Nothing

  let binopsP = fmap (PT_Binops . (t1:) . concat) (many1 bp)
      bp = do
        op <- try (lxs (pure ()) *> fmap PS_Var (lx operatorP))
        r <- simpleTermP termP
        return [op, r]

  try partialRightBinopP <|> binopsP <|> return (PT_Simple t1)


instance Show PTerm where
  show (PT_Simple x) = show x
  show (PT_Match t l) = "match (" <> show t <> ")" <> show l
  show (PT_IfElse l) = intercalate " " (map show l)
  show (PT_DoNotation doBody) = show doBody
  show (PT_Binops l) = intercalate " " $ map show l
  show (PT_PartialBinop l op r) = unwords [f l, show op, f r]
    where f Nothing = "_"
          f (Just x) = show x
  show (PT_Lambda args x) =
    "fn " <> "(" <> intercalate ", " (map show args)
    <> ") -> " <> show x

instance Show PTPattern where
  show (PTPattern x) = show x
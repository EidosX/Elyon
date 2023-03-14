-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Terms (
  PTerm (..), PTermPattern (..),
  termP, termPatternP
) where

import Elyon.Parser (Parser, lx, lxs)
import Elyon.Parser.Patterns (PPattern (..), patternP)
import Elyon.Parser.Terms.Simple (simpleTermP, PSimpleTerm (..),
  operatorP, argListP)
import Data.Functor (($>))
import Data.List (intercalate)
import Text.Parsec ((<|>), try, char, string, many1)
import Text.Parsec.Indent (indented)

data PTerm = 
    PT_Simple (PSimpleTerm PTerm)
  | PT_Lambda [PTermPattern] PTerm
  | PT_PartialBinop
      (Maybe (PSimpleTerm PTerm))
      (PSimpleTerm PTerm)
      (Maybe (PSimpleTerm PTerm))
  | PT_Binops [PSimpleTerm PTerm] -- 3 > x >= 12 becomes [3, >, x, >=, 12]
  deriving (Eq)

newtype PTermPattern =
  PTermPattern (PPattern (PSimpleTerm PTermPattern) PTerm)
  deriving (Eq)

termPatternP :: Parser PTermPattern
termPatternP = fmap PTermPattern $
  patternP (simpleTermP termPatternP) termP

termP :: Parser PTerm
termP = lambdaP
    <|> partialLeftBinopP 
    <|> manyBinopsP

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
  show (PT_Binops l) = intercalate " " $ map show l
  show (PT_PartialBinop l op r) = unwords [f l, show op, f r]
    where f Nothing = "_"
          f (Just x) = show x
  show (PT_Lambda args x) =
    "fn" <> "(" <> intercalate ", " (map show args)
    <> ") -> " <> show x

instance Show PTermPattern where
  show (PTermPattern x) = show x
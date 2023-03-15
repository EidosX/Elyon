-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.IfElse (
  ifElseP, PIfElse
) where

import Elyon.Parser (Parser, lx, lxs)
import Text.Parsec (try, char, string, many, between, optionMaybe)
import Text.Parsec.Indent (indented)
import Data.Maybe (maybeToList)

data PIfElse c v = PIf c v | PElif c v | PElse v
  deriving (Eq)

ifElseP :: Parser c -> Parser v -> Parser [PIfElse c v]
ifElseP condP valP = do
  let p1 ifP = do
        if' <- try (lx ifP) 
          *> lxs (between (lx $ char '(') (char ')') (lx condP))
        then' <- indented *> lxs valP
        return (if', then')
  x1 <- fmap (uncurry PIf) (p1 (string "if"))
  xs <- many (fmap (uncurry PElif) (p1 (string "elif")))
  end <- optionMaybe ((lxs $ try (string "else")) *> 
        indented *> fmap PElse valP)
  return $ (x1 : xs) ++ maybeToList end

instance (Show c, Show v) => Show (PIfElse c v) where
  show (PIf c v) = "if (" <> show c <> ") " <> show v
  show (PElif c v) = "elif (" <> show c <> ") " <> show v
  show (PElse v) = "else " <> show v
-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Lists (
  listManyLineP,
  listLiteralP,
  ListLiteralContent (..)
) where

import Elyon.Parser (Parser, lx, lxs)
import Text.Parsec (sepEndBy, char, between, (<|>), string)
import Text.Parsec.Indent (indented, checkIndent)

data ListLiteralContent e = LLCElem e
                          | LLCSpread e
  deriving (Eq)

instance Show e => Show (ListLiteralContent e) where
  show (LLCElem e) = show e
  show (LLCSpread e) = ".." <> show e

listManyLineP :: Parser begin -> Parser end -> Parser e -> Parser [e]
listManyLineP begin end p = between begin' end' $
  sepEndBy (indented *> lxs p) (indented *> lxs (char ','))
  where begin' = lxs begin
        end' = (checkIndent <|> indented) *> end

listLiteralP :: Parser e -> Parser [ListLiteralContent e]
listLiteralP p = listManyLineP (char '[') (char ']') p'
  where p' = fmap LLCSpread (lx (string "..") *> p)
             <|> fmap LLCElem p


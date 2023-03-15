-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.UseStatements (
  PUseStatement (..), useStmtP
) where

import Elyon.Parser (Parser, lx, lxs)
import Elyon.Parser.Terms.Simple (varP)
import Text.Parsec (sepBy1, sepBy, char, between, (<|>), 
  string, try)
import Text.Parsec.Indent (indented)
import Data.Text (Text)
import Data.Functor (($>))
import Data.List (intercalate)
import qualified Data.Text as T

data PUseStatement =
    PU_Use [Text] [PUseStatement]
  | PU_Module Text
  | PU_Identifier Text
  | PU_Wildcard
  deriving (Eq)

useStmtP :: Parser PUseStatement
useStmtP = do
  mods <- sepBy1 varP (char '.')
  items <- between (lxs $ char '{') (char '}') $
    (sepBy (indented *> lxs useStmtInsideP) 
           (indented *> lxs (char ',')))
  return $ PU_Use mods items

useStmtInsideP :: Parser PUseStatement
useStmtInsideP = try useStmtP 
             <|> (char '*' $> PU_Wildcard)
             <|> (lx (try $ string "mod ") *> fmap PU_Module varP)
             <|> (fmap PU_Identifier varP)
  

instance Show PUseStatement where
  show (PU_Use ms x) =
    intercalate "." (map T.unpack ms) <> "{" <>
    intercalate ", " (map show x) <> "}"
  show (PU_Identifier x) = T.unpack x
  show PU_Wildcard = "*"
  show (PU_Module x) = "mod " <> T.unpack x
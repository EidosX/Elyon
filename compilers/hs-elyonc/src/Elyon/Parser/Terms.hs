-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Terms (
  PSimpleTerm (..), PTerm (..), termP, simpleTermP
) where

import Elyon.Parser.Primitives (TemplateStringContent(..),
  Sign(..), floatP, intP, charP, templateStringP)
import Elyon.Parser (Parser, lx, lxs)
import Elyon.Parser.Lists (ListLiteralContent (..), listLiteralP)
import Elyon.Parser.Patterns (PPattern)
import Data.Text (Text)
import Data.List (intercalate)
import qualified Data.Text as T
import Text.Parsec ((<|>), try, oneOf, letter, digit, many, char,
  between, optionMaybe, sepEndBy)
import Text.Parsec.Indent (indented, checkIndent)
import Control.Applicative (liftA2)

data PSimpleTerm t = 
    PS_Var Text
  | PS_Float (Sign, Text, Text)
  | PS_Int (Sign, Text)
  | PS_Char Char
  | PS_String [TemplateStringContent t]
  | PS_List [ListLiteralContent t]
  | PS_Parens t
  | PS_Appl (PSimpleTerm t) [t]
  | PS_DefaultArgAppl (PSimpleTerm t) [(Maybe Text, t)]
  deriving (Eq)

data PTerm = 
    PT_Simple (PSimpleTerm PTerm)
  | PT_Binops [PTerm] -- 3 > x >= 12 becomes [3, >, x, >=, 12]
  | PT_PartialBinop (Maybe PTerm) PTerm (Maybe PTerm)
  | PT_Lambda [PPattern PTerm] PTerm
  deriving (Eq, Show)

argListP :: Parser begin -> Parser end -> Parser e -> Parser [e]
argListP begin end p = do
  _ <- try (lxs (pure ()) *> indented *> lxs begin)
  l <- sepEndBy (indented *> lxs p) (indented *> lxs (char ','))
  _ <- (indented <|> checkIndent) *> end
  return l

simpleTermP :: Parser p -> Parser (PSimpleTerm p)
simpleTermP p = do
  t1 <- fmap PS_Var varP
    <|> try (fmap PS_Float floatP)
    <|> fmap PS_Int intP
    <|> fmap PS_Char charP
    <|> fmap PS_String (templateStringP p)
    <|> fmap PS_List (listLiteralP p)
    <|> fmap PS_Parens (between (char '(') (char ')') p)

  let defaultArgP = 
        liftA2 (,) (try $ fmap Just(lx varP <* lx (char '='))) p
        <|> fmap (Nothing,) p
  defaultArgs <- optionMaybe $
    argListP (char '{') (char '}') defaultArgP
  let t2 = foldl PS_DefaultArgAppl t1 defaultArgs

  args <- many (argListP (char '(') (char ')') p)
  let t3 = foldl PS_Appl t2 args

  return t3
  

varP :: Parser Text
varP = fmap T.pack $ 
  liftA2 (:) letter (many (letter <|> digit <|> oneOf "_"))

termP :: Parser PTerm
termP = fmap PT_Simple (simpleTermP termP)



instance Show e => Show (PSimpleTerm e) where
  show (PS_Var x) = T.unpack x
  show (PS_Float (Plus, l, r)) = T.unpack (l <> "." <> r)
  show (PS_Float (Minus, l, r)) = T.unpack ("-" <> l <> "." <> r)
  show (PS_Int (Plus, x)) = T.unpack x
  show (PS_Int (Minus, x)) = T.unpack ("-" <> x)
  show (PS_Char c) = "'" <> [c] <> "'"
  show (PS_String ss) = "\"" <> concat (map f ss) <> "\""
    where f (ISCInterpolated x) = "{" <> show x <> "}"
          f (ISCString s) = T.unpack s
  show (PS_List l) = "[" <> intercalate ", " (map show l) <> "]"
  show (PS_Parens x) = "(" <> show x <> ")"
  show (PS_Appl l args) = 
    show l <> "(" <> intercalate ", " (map show args) <> ")"
  show (PS_DefaultArgAppl t args) = 
    show t <> "{" <> intercalate ", " (map f args) <> "}"
    where f (Nothing, v) = show v
          f (Just name, v) = T.unpack name <> " = " <> show v
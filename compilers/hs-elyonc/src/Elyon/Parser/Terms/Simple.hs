-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Terms.Simple (
  PSimpleTerm (..), simpleTermP, varP, operatorP, argListP
) where

import Elyon.Parser.Primitives (TemplateStringContent(..),
  Sign(..), floatP, intP, charP, templateStringP)
import Elyon.Parser (Parser, lx, lxs)
import Elyon.Parser.Lists (ListLiteralContent (..), listLiteralP)
import Data.Text (Text)
import Data.List (intercalate)
import Data.Char (isSymbol)
import Control.Monad (when)
import qualified Data.Text as T
import Text.Parsec ((<|>), try, oneOf, letter, digit, many, char,
  between, optionMaybe, sepEndBy, satisfy, many1, sepBy1)
import Text.Parsec.Indent (indented, checkIndent)
import Control.Applicative (liftA2)

reservedOperators :: [Text]
reservedOperators = ["->", "=", "=!", "!", ":", "_", ".", "//"]

reservedKeywords :: [Text]
reservedKeywords = ["use", "mod", "trait", 
  "def", "dat", "let", "named",
  "match", "if", "elif", "else", "do", "fn", "ret"
  ]

data PSimpleTerm t = 
    PS_Var Text
  | PS_ModuleName [Text] (PSimpleTerm t)
  | PS_Float (Sign, Text, Text)
  | PS_Int (Sign, Text)
  | PS_Char Char
  | PS_String [TemplateStringContent t]
  | PS_List [ListLiteralContent t]
  | PS_BangBind (PSimpleTerm t)
  | PS_Parens t
  | PS_Appl (PSimpleTerm t) [t]
  | PS_DefaultArgAppl (PSimpleTerm t) [(Maybe Text, t)]
  deriving (Eq)
argListP :: Parser begin -> Parser end -> Parser e -> Parser [e]
argListP begin end p = do
  _ <- try (lxs (pure ()) *> indented *> lxs begin)
  l <- sepEndBy (indented *> lxs p) (indented *> lxs (char ','))
  _ <- (indented <|> checkIndent) *> end
  return l

simpleTermP :: Parser p -> Parser (PSimpleTerm p)
simpleTermP p = do
  t1 <- try (fmap PS_Float floatP)
    <|> fmap PS_Int intP
    <|> fmap PS_Char charP
    <|> fmap PS_String (templateStringP p)
    <|> fmap PS_List (listLiteralP p)
    <|> fmap PS_Parens (between (char '(') (char ')') p)
    <|> identifierP
    <|> fmap PS_BangBind (bangBindP p)

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
varP = try $ do 
  var <- fmap T.pack $ 
    liftA2 (:) letter (many (letter <|> digit <|> oneOf "_"))
  when (var `elem` reservedKeywords) (fail "Reserved Keyword")
  return var

operatorP :: Parser Text
operatorP = try $ do
  op <- fmap T.pack $ 
    many1 (satisfy isSymbol <|> oneOf "+-=<>?!*:.$§&@#%/\\")
  when (op `elem` reservedOperators) (fail "Reserved Operator")
  return op

identifierP :: forall p. Parser (PSimpleTerm p)
identifierP = fmap PS_Var operatorP <|> do
  l <- sepBy1 (varP <|> operatorP) (char '.')
  let (mods, var) = (init l, PS_Var $ last l)
  return $ if null mods then var else (PS_ModuleName mods var)

bangBindP :: Parser e -> Parser (PSimpleTerm e)
bangBindP p = char '!' *> simpleTermP p


instance Show e => Show (PSimpleTerm e) where
  show (PS_Var x) = T.unpack x
  show (PS_ModuleName mods t) = 
    intercalate "." (map T.unpack mods) <> "." <> show t
  show (PS_Float (Plus, l, r)) = T.unpack (l <> "." <> r)
  show (PS_Float (Minus, l, r)) = T.unpack ("-" <> l <> "." <> r)
  show (PS_Int (Plus, x)) = T.unpack x
  show (PS_Int (Minus, x)) = T.unpack ("-" <> x)
  show (PS_Char c) = "'" <> [c] <> "'"
  show (PS_String ss) = "\"" <> concat (map f ss) <> "\""
    where f (ISCInterpolated x) = "{" <> show x <> "}"
          f (ISCString s) = T.unpack s
  show (PS_List l) = "[" <> intercalate ", " (map show l) <> "]"
  show (PS_BangBind x) = '!' : show x
  show (PS_Parens x) = "(" <> show x <> ")"
  show (PS_Appl l args) = 
    show l <> "(" <> intercalate ", " (map show args) <> ")"
  show (PS_DefaultArgAppl t args) = 
    show t <> "{" <> intercalate ", " (map f args) <> "}"
    where f (Nothing, v) = show v
          f (Just name, v) = T.unpack name <> " = " <> show v

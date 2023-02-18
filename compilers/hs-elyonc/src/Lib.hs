-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Lib (
  Term(..), Pattern(..), 
  (<.<),
  intToTerm, floatToTerm, charToTerm, listToTerm, stringToTerm
) where

data Term = TermVar String
          | TermLambda Pattern Term
          | TermAppl Term Term
  deriving (Eq)

data Pattern = PatternVar String
             | PatternCondition Pattern Term
             | PatternMatch Term Term
  deriving (Show, Eq)

(<.<) :: Term -> Term -> Term
(<.<) = TermAppl
infixl 6 <.<

instance Show Term where
  show (TermVar s) = s
  show (TermLambda x t) = "fn " <> show x <> " -> " <> show t
  show (TermAppl t1 t2) = show t1 <> "(" <> show t2 <> ")"

intToTerm :: String -> Term
intToTerm x = TermVar x -- TODO

floatToTerm :: (String, String) -> Term
floatToTerm (int, dec) = TermVar (int <> "." <> dec) -- TODO

charToTerm :: Char -> Term
charToTerm c = TermVar [c] -- TODO

listToTerm :: [Term] -> Term
listToTerm [] = TermVar "data.list.Nil"
listToTerm (x:xs) = TermVar "data.list.Cons" <.< x <.< listToTerm xs

stringToTerm :: String -> Term
stringToTerm = listToTerm . map charToTerm
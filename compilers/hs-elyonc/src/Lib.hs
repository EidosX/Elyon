-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Lib (
  Term(..), Pattern(..), 
  (<.<),
  intToTerm, floatToTerm, charToTerm, listToTerm, stringToTerm
) where

import Data.List (intercalate)
import Control.Arrow (first, second)

data Term = TermVar String
          | TermLambda Pattern Term
          | TermAppl Term Term
  deriving (Eq)

data Pattern = PatternCondition Pattern Term
             | PatternMatch Term
  deriving (Show, Eq)

(<.<) :: Term -> Term -> Term
(<.<) = TermAppl
infixl 6 <.<

uncurryAppl :: Term -> (Term, [Term])
uncurryAppl (TermAppl t1 t2) = second (++ [t2]) (uncurryAppl t1)
uncurryAppl t = (t, [])

uncurryLambda :: Term -> ([Pattern], Term)
uncurryLambda (TermLambda x t) = first (x :) (uncurryLambda t)
uncurryLambda t = ([], t)

instance Show Term where
  show (TermVar s) = s
  show t@(TermLambda _ _) = "((" <> intercalate ", " (map show args) <> ") -> " <> show res <> ")"
    where (args, res) = uncurryLambda t
  show t@(TermAppl _ _) = show fn <> "(" <> intercalate ", " (map show args) <> ")"
    where (fn, args) = uncurryAppl t

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
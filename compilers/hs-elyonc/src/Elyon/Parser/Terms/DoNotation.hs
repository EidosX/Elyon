-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Terms.DoNotation (
  DoNotationContent (..), DoNotationBody (..),
  doNotationP
) where

import Elyon.Parser (Parser)

data DoNotationBody patt term = DoNotationBody {
  dnb_monads :: [term],
  dnb_content :: DoNotationContent patt term
} deriving (Eq, Show)

data DoNotationContent patt term =
    DNC_DiscardBind term
  | DNC_Bind patt term
  | DNC_Assign patt term
  | DNC_Ret term
  | DNC_ElIf [(term, DoNotationContent patt term)]
  deriving (Eq, Show)

doNotationP :: Parser p -> Parser t -> Parser (DoNotationBody p t)
doNotationP patt term = do
  fail "..."
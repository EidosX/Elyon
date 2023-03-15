-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Terms.DoNotation (
  DoNotationContent (..), DoNotationBody (..),
  doNotationP
) where

import Elyon.Parser (Parser, lxs, lx)
import Elyon.Parser.Assignments (PAssignment, assignmentWithBodyP)
import Text.Parsec (optionMaybe, string, sepEndBy, between, 
  char, string, try, (<|>))
import Text.Parsec.Indent (indented, block)

data DoNotationBody patt term = DoNotationBody {
  dnb_monads :: Maybe [term],
  dnb_content :: [DoNotationContent patt term]
} deriving (Eq, Show)

data DoNotationContent patt term =
    DNC_DiscardBind term
  | DNC_Bind (PAssignment patt term)
  | DNC_Assign (PAssignment patt term)
  | DNC_Ret term
  deriving (Eq, Show)

doNotationP :: Parser p -> Parser t -> Parser (DoNotationBody p t)
doNotationP patt term = do
  let monadsListP = between (lx $ char '{') (char '}') $
        sepEndBy (lx term) (lx $ char ',')
  monads <- lx (try $ string "do") *> try (optionMaybe monadsListP)
  lxs $ pure ()
  content <- indented *> block (lxs $ doContentP patt term)
  return $ DoNotationBody {
    dnb_monads = monads,
    dnb_content = content
  }

doContentP :: Parser p -> Parser t -> Parser (DoNotationContent p t)
doContentP pattP termP =
      fmap DNC_Ret (try (lx (string "ret ")) *> termP)
  <|> try (fmap DNC_Bind 
        (assignmentWithBodyP pattP termP (string "=!")))
  <|> try (fmap DNC_Assign 
        (assignmentWithBodyP pattP termP (char '=')))
  <|> fmap DNC_DiscardBind termP

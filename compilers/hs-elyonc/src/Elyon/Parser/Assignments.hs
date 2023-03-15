-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Assignments (
  PAssignment (..), assignmentP
) where

import Elyon.Parser (Parser, lx, lxs)
import Text.Parsec (char, (<|>), optionMaybe, string, try)
import Text.Parsec.Indent (indented, withPos, block)
import Data.Text (Text)
import Data.Functor (($>))
import Data.List (intercalate)
import qualified Data.Text as T
import Elyon.Parser.Terms.Simple (varP, operatorP, argListP)
import Elyon.Parser.UseStatements (useStmtP, PUseStatement)

data DefaultArgNamed = DAN_Named | DAN_Positional
  deriving (Eq, Show)

data PAssignment patt term = PA_FuncAssignment {
  paa_funcName :: Text,
  paa_defaultArgs :: [(DefaultArgNamed, patt, Maybe term)],
  paa_args :: [patt],
  paa_retPattern :: Maybe patt,
  paa_body :: Maybe term,
  paa_letBindings :: [(PAssignment patt term)],
  paa_useStmts :: [PUseStatement]
} deriving (Eq)

assignmentP :: Parser patt -> Parser term 
            -> Parser (PAssignment patt term)
assignmentP pattP termP = withPos $ do
  funcName <- lx (varP <|> operatorP)

  defaultArgs <- fmap concat . optionMaybe . lxs $ 
    argListP (char '{') (char '}') (defaultArgP pattP termP)

  args <- fmap concat . optionMaybe . lxs $ 
    argListP (char '(') (char ')') pattP

  retPattern <- optionMaybe . lxs $
    (indented *> lxs (char ':') *> indented *> (lxs pattP))

  body <- optionMaybe
    (indented *> try (lxs (char '=')) *> indented *> termP)

  let letBindingsP = do
        _ <- try (lxs (pure ()) *> indented *> lxs (string "let "))
        indented *> block (lxs (assignmentP pattP termP))
  letBindings <- letBindingsP <|> return []

  let useStmtsP = do
        _ <- try (lxs (pure ()) *> indented *> lxs (string "use "))
        indented *> block (lxs useStmtP)
  useStmts <- useStmtsP <|> return []

  return $ PA_FuncAssignment {
    paa_funcName = funcName,
    paa_defaultArgs = defaultArgs,
    paa_args = args,
    paa_retPattern = retPattern,
    paa_body = body,
    paa_letBindings = letBindings,
    paa_useStmts = useStmts
  }

defaultArgP :: Parser patt -> Parser term 
            -> Parser (DefaultArgNamed, patt, Maybe term)
defaultArgP pattP termP = do
  dan <- try (lx $ string "named " $> DAN_Named) 
         <|> return DAN_Positional
  patt <- pattP
  term <- optionMaybe (try (lx (pure ()) *> 
                       lx (string "=") *> termP))
  return (dan, patt, term)

instance (Show patt, Show term) => Show (PAssignment patt term) where
  show (PA_FuncAssignment 
          funcName defArgs args retPattern 
          body letBindings useStmts) =
    T.unpack funcName <> defArgs' <> args' <> retPatt' 
      <> body' <> let' <> use'
    where defArgs' = if null defArgs then "" else 
            "{" <> intercalate ", " (map show defArgs) <> "}"
          args' = if null args then ""  else 
            "(" <> intercalate ", " (map show args) <> ")"
          retPatt' = case retPattern of Nothing -> ""
                                        Just s -> ": " <> show s
          body' = case body of Nothing -> ""
                               Just b  -> " = " <> show b
          let' = if null letBindings then ""
                 else " let " <> show letBindings
          use' = if null useStmts then ""
                 else " use " <> show useStmts

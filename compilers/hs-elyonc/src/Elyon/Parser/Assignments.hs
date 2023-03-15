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
import Elyon.Parser.Terms (PTPattern, PTerm, termPatternP, termP)
import Elyon.Parser.Terms.Simple (varP, operatorP, argListP)
import Elyon.Parser.UseStatements (useStmtP, PUseStatement)

data DefaultArgNamed = DAN_Named | DAN_Positional
  deriving (Eq, Show)

data PAssignment = PA_FuncAssignment {
  paa_funcName :: Text,
  paa_defaultArgs :: [(DefaultArgNamed, PTPattern, Maybe PTerm)],
  paa_args :: [PTPattern],
  paa_retPattern :: Maybe PTPattern,
  paa_body :: Maybe PTerm,
  paa_letBindings :: [PAssignment],
  paa_useStmts :: [PUseStatement]
} deriving (Eq)

assignmentP :: Parser PAssignment
assignmentP = withPos $ do
  funcName <- lx (varP <|> operatorP)

  defaultArgs <- fmap concat . optionMaybe . lxs $ 
    argListP (char '{') (char '}') defaultArgP

  args <- fmap concat . optionMaybe . lxs $ 
    argListP (char '(') (char ')') termPatternP

  retPattern <- optionMaybe . lxs $
    (indented *> lxs (char ':') *> indented *> (lxs termPatternP))

  body <- optionMaybe
    (indented *> try (lxs (char '=')) *> indented *> termP)

  let letBindingsP = do
        _ <- try (lxs (pure ()) *> indented *> lxs (string "let "))
        indented *> block (lxs assignmentP)
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

defaultArgP :: Parser (DefaultArgNamed, PTPattern, Maybe PTerm)
defaultArgP = do
  dan <- try (lx $ string "named " $> DAN_Named) 
         <|> return DAN_Positional
  patt <- termPatternP
  term <- optionMaybe (try (lx (pure ()) *> 
                       lx (string "=") *> termP))
  return (dan, patt, term)

instance Show PAssignment where
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

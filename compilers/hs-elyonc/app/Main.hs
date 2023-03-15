-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Elyon.Parser (quickParse)
import Elyon.Parser.File (fileP)

main :: IO ()
main = do
  args <- getArgs
  let path = last args
  source <- TIO.readFile path
  case quickParse fileP source of
    Left err -> print err
    Right l -> mapM_ print l
  return ()

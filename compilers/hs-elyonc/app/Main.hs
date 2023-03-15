-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  let path = last args
  source <- TIO.readFile path
  print source
  return ()

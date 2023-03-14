-- Copyright (c) 2023 Diego Imbert
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Elyon.Parser.Patterns (
  PPattern (..)
) where

data PPattern t = 
    PP_Match t
  | PP_Wildcard
  | PP_Cond (PPattern t) t
  deriving (Eq)

instance Show t => Show (PPattern t) where
  show (PP_Match t) = show t
  show (PP_Wildcard) = "_"
  show (PP_Cond p t) = show p <> ": " <> show t

{-# LANGUAGE DeriveGeneric #-}

module Constraint (
  Constraint(..),
  parse,
  parse'
) where

import Data.Char

data Constraint = Constraint {
  name :: String,
  install :: Bool,
  version :: String,
  operator :: String
} deriving (Show)

parse :: Bool -> String -> Constraint
parse install s = Constraint name install version operator
  where
    version = snd $ splitAt (length name + length operator) s
    operator = takeWhile (not . isNumber) (snd $ splitAt (length name) s)
    name = takeWhile (\c -> all (/=c) ['<', '>', '=']) s
    
parse' :: String -> Constraint
parse' (s:ss) = case s of
  '-' -> parse False ss
  '+' -> parse True ss

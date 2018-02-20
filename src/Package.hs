{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Package (
  Package(..),
  getConflicts,
  getDepends
) where

import GHC.Generics (Generic)
import Control.Parallel.Strategies (NFData)
import Data.Aeson
import qualified Constraint as C

data Package = Package {
  name :: String,
  version :: String,
  size :: Int,
  conflicts :: Maybe [String],
  depends :: Maybe [[String]]
} deriving (Generic, Show, Eq, NFData)

instance ToJSON Package
instance FromJSON Package

getConflicts :: Package -> [C.Constraint]
getConflicts package = case conflicts package of
  Nothing -> []
  Just c -> map (C.parse False) c

getDepends :: Package -> [[C.Constraint]]
getDepends package = case depends package of
  Nothing -> []
  Just d -> map (map (C.parse True)) d
  

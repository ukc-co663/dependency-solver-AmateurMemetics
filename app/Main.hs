
module Main (main) where

import System.Environment
import Control.Parallel.Strategies
import Data.Aeson
import Data.Maybe
import Data.List
import Debug.Trace
import qualified Package as P
import qualified Constraint as C
import qualified Data.ByteString.Lazy.Char8 as BL

maxWith :: (Ord b) => (a -> b) -> [a] -> a
maxWith f xs =
  snd $ foldl (\(i, a) b -> let i' = f b in
                if i > i'
                then (i, a)
                else (i', b)) (f $ head xs, head xs) xs

minWith :: (Show a, Ord b) => (a -> b) -> [a] -> a
minWith f xs =
  snd $ foldl (\(i, a) b -> let i' = f b in
                if i < i'
                then (i, a)
                else (i', b)) (f $ head xs, head xs) xs

compareWith :: (Show a, Ord a) => String -> a -> a -> Bool
compareWith operator a b = case operator of
  "<" -> a < b
  ">" -> a > b
  "<=" -> a <= b
  ">=" -> a >= b
  "=" -> a == b
  "" -> True
  s -> error s

packageToCommand :: (P.Package, Bool) -> String
packageToCommand (p, install) = inst ++ (P.name p) ++ "=" ++ (P.version p)
  where inst = if install then "+" else "-"

stripPackages :: [(P.Package, Bool)] -> [(P.Package, Bool)] -> [(P.Package, Bool)]
stripPackages initial ps = filter f ps where
  f p = if snd p then not $ p `elem` initial else (fst p, True) `elem` initial

constraintsToPackages' :: [P.Package] -> [C.Constraint] -> [([P.Package], Bool)]
constraintsToPackages' ps cs = map f $ cs where
  f c = (
    filter (\p -> (C.name c) == (P.name p) && compareWith (C.operator c) (P.version p) (C.version c)) ps,
    C.install c)
  
constraintsToPackages :: [P.Package] -> [C.Constraint] -> [P.Package]
constraintsToPackages ps cs = concat . map f $ cs where
  f c = filter (\p -> (C.name c) == (P.name p) && compareWith (C.operator c) (P.version p) (C.version c)) ps

installPackages :: [P.Package] -> [P.Package] -> [([P.Package], Bool)] -> (Int, [(P.Package, Bool)])
installPackages repository initial constraints =
  let maxCost = 10000000000000 in
  let g p = (p, True) in
  let f path repo cost install package
        | package `elem` path = (maxCost, repo)
        | install && installed = (cost, repo)
        | install && not installed =
          let cs = constraintsToPackages repository $ P.getConflicts package in
          let ds = map (constraintsToPackages repository) $ P.getDepends package in
          let (cost', repo') = foldl removeConflicts (cost, repo) cs in
          let (cost'', repo'') = foldl installDependencies (cost', repo') ds in
            (if required then maxCost else cost'' + (P.size package), (package, True) : repo'')
        | not install && installed = (if required then maxCost else cost + 1000000, (package, False) : repo)
        | not install && not installed = (cost, repo)
        where
          removeConflicts (cost, repo) c = f (package : path) repo cost False c
          installDependencies (cost, repo) ds = if ds == []
            then (cost, repo)
            else
            let deps = (map (f (package : path) repo cost True) ds) `using` parList rdeepseq in
              minWith fst deps
          installed = (package, True) `elem` repo
          required = elem package $ concat $ map fst $ filter (\c -> snd c /= install) constraints in
  -- todo: remove code duplication
  let removeConflicts (cost, repo) c = f [] repo cost False c in
  let installDependencies (cost, repo) ds = if ds == []
        then (cost, repo)
        else
        let deps = (map (f [] repo cost True) ds) `using` parList rdeepseq in
          minWith fst deps in
    foldl (\acc (ps, inst) -> if inst
            then foldl installDependencies acc [ps]
            else foldl removeConflicts acc ps) (0, map g initial) constraints
    
getCommands :: [P.Package] -> [C.Constraint] -> [C.Constraint] -> [String]
getCommands repository constraints initial =
  map packageToCommand $ reverse $ stripPackages (map (\i -> (i, True)) is) $ snd $ installPackages repository is cs
  where
    cs = constraintsToPackages' repository constraints
    is = constraintsToPackages repository initial

-- Main Program

maybeListToList :: Maybe [a] -> [a]
maybeListToList x = case x of
  Nothing -> []
  Just xs -> xs

main :: IO ()
main = do
  args <- getArgs
  r <- BL.readFile $ args !! 0
  i <- BL.readFile $ args !! 1
  c <- BL.readFile $ args !! 2
  let repository = maybeListToList $ decode r :: [P.Package]
  let constraints = map C.parse' $ maybeListToList $ decode c :: [C.Constraint]
  let initial = map (C.parse True) $ maybeListToList $ decode i :: [C.Constraint]
  BL.putStrLn $ encode $ getCommands repository constraints initial

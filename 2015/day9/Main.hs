module Main where

import           AOC.IO
import           Data.List (permutations, union)
import qualified Data.Map.Strict as M (Map, empty, fromList, findWithDefault, union)

type City = String
type AdjList = M.Map (City,City) Int

main :: IO ()
main = do
  (minPath, maxPath) <- processInput hamiltonianPath
  putStr "Part 1: " >> print minPath
  putStr "Part 2: " >> print maxPath

toAdjacencyList :: [String] -> ([City], AdjList)
toAdjacencyList = foldr toAdj ([], M.empty) where
  toAdj l acc = let arr = words l
                    c1 = head arr
                    c2 = arr !! 2
                    dist = read (arr !! 4) :: Int
                    cities = [c1, c2] `union` fst acc
                    adj = M.fromList [((c1, c2), dist), ((c2, c1), dist)] `M.union` snd acc
                    in (cities, adj)

calculatePathCost :: AdjList -> Int -> [City] -> Int
calculatePathCost _ c [] = c
calculatePathCost paths c cities@(_:xs) = case take 2 cities of
  [c1, c2] -> c + M.findWithDefault 0 (c1, c2) paths + calculatePathCost paths c xs
  _ -> c

hamiltonianPath :: [String] -> (Int, Int)
hamiltonianPath lns = let (cities, adj) = toAdjacencyList lns
                          paths = map (calculatePathCost adj 0) (permutations cities)
                      in (minimum paths, maximum paths)



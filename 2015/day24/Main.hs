module Main where

import AOC.IO
import Data.List (tails)

main :: IO ()
main = do
  packages <- processInput $ map (\x -> read x :: Int)
  putStrLn "Part 1: "
  print $ findConfig packages 3
  putStrLn "Part 2: "
  print $ findConfig packages 4

subsequences :: Int -> [a] -> [[a]]
subsequences 0 _  = [[]]
subsequences n items = [x:ys | x:xs <- tails items,
                         ys <- subsequences (n-1) xs]


findConfig :: [Int] -> Int -> Int
findConfig packages n = quantumEntanglement where
  groupSize = sum packages `div` n
  configs = [sub |
              x <- [2..length packages],
              sub <- subsequences x packages,
              sum sub == groupSize]
  quantumEntanglement = product (head configs)

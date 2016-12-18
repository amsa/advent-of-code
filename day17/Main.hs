module Main where

import AOC.IO
import Data.List (tails)

main :: IO ()
main = do
  putStr "Part 1: "
  numbers <- processInput toInt --[20, 15, 10, 5, 5]
  let result = combinations 150 numbers
  print $ length result
  putStr "Part 2: "
  let minContainers = minimum (map length result)
  print $ length $ filter ((minContainers==) . length) result

toInt :: [String] -> [Int]
toInt = map (\x -> read x :: Int)

subsequences' :: Int -> [a] -> [[a]]
subsequences' 0 _  = [[]]
subsequences' n items = [x:ys | x:xs <- tails items,
                         ys <- subsequences' (n-1) xs]

combinations :: Int -> [Int] -> [[Int]]
combinations capacity containers =
  concatMap combinations' [2..length containers+1] where
  combinations' n = filter (\c -> sum c == capacity) results where
    results = subsequences' n containers




module Main where

import Data.List (nub)

main :: IO ()
main = do
  putStr "Part 1: "
  print $ part1 34000000
  putStr "Part 2: "
  print $ part2 34000000

part1 :: Int -> (Int, Int)
part1 n = findLowest n numbers where
  numbers      = map (sum . map (*10) . factors') [1..]

part2 :: Int -> (Int, Int)
part2 n = findLowest n numbers where
  numbers      = map (sum . take 50 . map (*11) . factors') [1..]

findLowest :: Int -> [Int] -> (Int, Int)
findLowest n housePresents = findHouse (zip [1..] housePresents) where
  findHouse xs = head $ dropWhile ((< n) . snd) xs

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

factors' :: Int -> [Int]
factors' n = nub $ concat [[x, n `div` x] |
                     x <- [1.._sqrt n], n `mod` x == 0] where
  _sqrt = ceiling . (sqrt :: Float -> Float) . fromIntegral

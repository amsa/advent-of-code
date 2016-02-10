module Main where

import Data.List (group)

main :: IO ()
main = do
  putStr "Part 1: "
  print $ length (lookAndSay "3113322113" 40)
  putStr "Part 2: "
  print $ length (lookAndSay "3113322113" 50)


lookAndSay :: String -> Int -> String
lookAndSay s = lookAndSay' s 0

lookAndSay' :: String -> Int -> Int -> String
lookAndSay' s i mx = let enc e = show (length e) ++ [head e]
                         result = if i >= mx
                                  then s
                                  else lookAndSay' (concatMap enc (group s)) (i+1) mx
                     in result

module Main where

import Data.Char (chr, ord)
import Data.List (intersect, nub, group, partition)

main :: IO ()
main = do
  putStr "Part 1: "
  let newpass = findNextPassword "cqjxjnds"
  print newpass
  putStr "Part 2: "
  print $ findNextPassword newpass

findNextPassword :: String -> String
findNextPassword str = let newpass = next str in
                           if req1 newpass && req2 newpass && req3 newpass
                           then newpass
                           else findNextPassword newpass

next :: String -> String
next str = reverse $ nx (reverse str) "" where
  nx :: String -> String -> String
  nx [] _ = []
  nx ('z':ss) acc = nx ss ('a':acc)
  nx (s:ss) acc = acc ++ chr(ord s + 1):ss

req1 :: String -> Bool
req1 str = isSeq charCodes where
  charCodes = map ord str
  isSeq :: [Int] -> Bool
  isSeq (x:y:z:zs) = x+1 == y && y+1 == z || isSeq (y:z:zs)
  isSeq _ = False

req2 :: String -> Bool
req2 str = null $ str `intersect` "ilo"

req3 :: String -> Bool
req3 str = let p = partition (\e -> length e >= 2) (group str)
               samples = nub $ map head (fst p)
           in length samples >= 2

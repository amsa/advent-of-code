module Main where

import           AOC.IO


main :: IO ()
main = do
  charCodes <- processInput countCharCodes
  chars <- processInput countChars
  putStrLn $ "Part 1: " ++  show (charCodes - chars)
  encChars <- processInput countEncChars
  putStrLn $ "Part 2: " ++  show (encChars - charCodes)

countCharCodes :: [String] -> Int
countCharCodes strs = sum (map length strs)

countChars :: [String] -> Int
countChars strs = sum $ map (countOne (-2)) strs where
  countOne :: Int -> String -> Int
  countOne acc [] = acc
  countOne acc ('\\':'x':_:_:xs) = countOne (acc+1) xs
  countOne acc ('\\':_:xs) = countOne (acc+1) xs
  countOne acc (_:xs) = countOne (acc+1) xs

countEncChars :: [String] -> Int
countEncChars strs = sum $ map (countEnc 2) strs where
  countEnc :: Int -> String -> Int
  countEnc acc [] = acc
  countEnc acc ('\\':xs) = countEnc (acc+2) xs
  countEnc acc ('"':xs) = countEnc (acc+2) xs
  countEnc acc (_:xs) = countEnc (acc+1) xs


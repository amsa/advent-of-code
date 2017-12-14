module Main where

import           Data.List
import           AOC.IO

main :: IO ()
main = do
  result <- processInput $ eachLine countLevels
  putStr $ unlines result


countLevels :: String -> (Integer, Maybe Int)
countLevels lst =
    let s =  scanl1 (+) $ toNum lst
        pos = (+1) <$> findIndex (< 0) s
    in (last s, pos)


toNum :: String -> [Integer]
toNum (x:xs) = (case x of
                   '(' -> 1
                   ')' -> -1
                   _ -> 0)
                   :toNum xs
toNum _    = []

module Main where

import           Text.Regex.PCRE
import           AOC.IO

main :: IO ()
main = do
  putStr "#Nice Strings 1: "
  r1 <- processInput $ countNice isNice1
  putStrLn r1
  putStr "#Nice Strings 2: "
  r2 <- processInput $ countNice isNice2
  putStrLn r2


isNice1 :: String -> Bool
isNice1 str = not (str =~ "(ab|cd|pq|xy)") &&
    str =~ "[aeoiu].*[aeoiu].*[aeoiu]" && str =~ "(.)\\1"


isNice2 :: String -> Bool
isNice2 str = str =~ "(.)(.).*\\1\\2" && str =~ "(.).\\1"


countNice :: (a -> Bool) -> [a] -> String
countNice fn input = show $ length $ filter fn input

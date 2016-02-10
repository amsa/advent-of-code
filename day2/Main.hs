module Main where

import           Data.List
import qualified Data.Text as T
import qualified Data.Text.Read as Tr
import           AOC.IO

main :: IO ()
main = do
  r1 <- processInput $ total calculateSurface
  putStrLn "Surface: "
  putStrLn r1

  r2 <- processInput $ total calculateRibbon
  putStrLn "Ribbon: "
  putStrLn r2


square :: ([Int] -> Int) -> String -> Int
square calculator dimStr =
    let arr = T.splitOn (T.singleton 'x') (T.pack dimStr)
        sides = sort $ map (\e -> case Tr.decimal e of
                                       Right (n, _) -> n
                                       Left _  -> 0) arr
    in calculator sides


calculateSurface :: [Int] -> Int
calculateSurface (a:b:c:_) = 2 * a * b + 2 * a * c + 2 * b * c + a * b
calculateSurface _ = 0

calculateRibbon :: [Int] -> Int
calculateRibbon (a:b:c:_) = a + a + b + b + (a * b * c)
calculateRibbon  _ = 0


total :: ([Int] -> Int) -> [String] -> String
total calculator input = show $ sum $ map (square calculator) input

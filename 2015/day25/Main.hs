module Main where

main :: IO ()
main = do
  putStrLn "Part 1: "
  print $ findCode 2947 3029

findCode :: Int -> Int -> Int
findCode x y = compute idx 20151125 where
  idx = sum [1..(x+y-2)] + y
  compute :: Int -> Int -> Int
  compute 1 code   = code
  compute num code = compute (num-1) newCode where
    multiplier = 252533
    divisor    = 33554393
    newCode    = (code * multiplier) `mod` divisor


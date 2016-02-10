module Main where

import           Data.List
import           AOC.IO

type Point = (Int, Int)
type Steps = [Point]
type Directions = String

main :: IO ()
main = do
  putStrLn "Part 1: "
  r1 <- processInput $ eachLine moves
  putStr $ unlines r1

  putStrLn "Part 2: "
  r2 <- processInput $ eachLine sumMove
  putStr $ unlines r2


moves :: Directions -> Int
moves = length . nub . move

move :: Directions -> Steps
move dirStr = let start = (0, 0) in move' start dirStr [start]

move' :: Point -> Directions -> Steps -> Steps
move' p@(x, y) (m:ms) s =
  let current = case m of
        '^' -> (x, y+1)
        'v' -> (x, y-1)
        '>' -> (x+1, y)
        '<' -> (x-1, y)
        _ -> p
  in current : move' current ms s
move' _ _ s  = s


 -- part 2
sumMove :: Directions -> Int
sumMove dirStr =
  let (sDir, rDir) = split dirStr
  in length $ nub (move sDir `union` move rDir)

split :: Directions -> (Directions, Directions)
split [] = error "No direction given."
split dirStr =
      let pairs = zip [(0 :: Integer)..] dirStr
          res = foldr (\(i, e) (_1, _2) -> if even i
                                           then (e:_1, _2)
                                           else (_1, e:_2)) ([], []) pairs
      in res

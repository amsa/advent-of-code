module Main where

import AOC.IO
import Data.Array.Unboxed

type Grid = Array (Int, Int) Bool

main :: IO ()
main = do
  grid <- processInput toGrid
  putStr "Part 1: "
  print $ solve grid next 100
  putStr "Part 2: "
  print $ solve (turnOnCorners grid) (turnOnCorners . next) 100


toGrid :: [String] -> Grid
toGrid input =
  array ((0, 0),(bound, bound)) initList where
  bound = length input - 1
  initList = concatMap decodeRow (zip [0..] input)
  decodeRow (i, line) = zipWith (curry decodeCell) [0..] line where
    decodeCell (j, '#') = ((i,j), True)
    decodeCell (j, _) = ((i,j), False)


turnOnCorners :: Grid -> Grid
turnOnCorners grid = let ((x0, y0), (x1, y1)) = bounds grid
                         corners = [(x0, y0),
                                    (x0, y1),
                                    (x1, y0),
                                    (x1, y1)]
                     in grid // [(p, True) | p <- corners]


next :: Grid -> Grid
next grid =
  let assoc = assocs grid
      bounds' = bounds grid
      new = map toggle assoc
      toggle (x, True)  = (x, stayOn x)
      toggle (x, False) = (x, turnOn x)

      stayOn :: (Int, Int) -> Bool
      stayOn x = onNeighbors x `elem` [2,3]
      turnOn :: (Int, Int) -> Bool
      turnOn x = onNeighbors x == 3

      onNeighbors (i, j) = length $ filter (True==) neighbors where
        neighbors = map (grid !) idxs
        idxs = [(x,y) | x <- [i-1..i+1], y <- [j-1..j+1],
                (x,y) /= (i,j), inRange bounds' (x,y)]
  in array bounds' new

solve :: Grid -> (Grid -> Grid) -> Int -> Int
solve grid _ 0 = length $ filter (True==) (elems grid)
solve grid f n = solve (f grid) f (n-1)

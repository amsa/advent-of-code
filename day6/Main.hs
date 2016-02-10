module Main where

import           AOC.IO
import           Text.Regex (mkRegex, matchRegex)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M


type Point = (Int, Int)
type Grid = M.Map Point Int

data Instruction = Null
     | Toggle Point Point
     | On Point Point
     | Off Point Point
     deriving (Eq, Show)

main :: IO ()
main = do
  r1 <- processInput part1
  putStrLn $ "Part 1:" ++ r1
  r2 <- processInput part2
  putStrLn $ "Part 2:" ++ r2

grid :: Int -> Int -> Grid
grid x y = M.fromList [((i,j), 0) | i <- [0..x], j <- [0..y]]

parseInstruction :: String -> Instruction
parseInstruction instruction =
                 let matches = matchRegex (mkRegex "(on|off|toggle) ([0-9,]+) through ([0-9,]+)") instruction
                     strToTuple str = let [x, y] = splitOn "," str
                       in (read x, read y)
                     inst = case matches of
                            Just ["toggle", start, end] -> Toggle (strToTuple start) (strToTuple end)
                            Just ["on", start, end]     -> On (strToTuple start) (strToTuple end)
                            Just ["off", start, end]    -> Off (strToTuple start) (strToTuple end)
                            _ -> Null
                     in inst

updateGrid :: Grid -> Point -> Point -> (Int -> Int) -> Grid
updateGrid g (sx, sy) (ex, ey) fn = M.foldrWithKey (\k@(x, y) v acc ->
        if sx <= x && x <= ex && sy <= y && y <= ey
        then M.insert k (fn v) acc
        else acc) g g

-- part 1
countOn :: Grid -> [String] -> Int
countOn g [] = M.size $ M.filter (==1) g
countOn g (x:xs) = let inst = parseInstruction x
                       newGrid = applyInstruction1 g inst
                   in countOn newGrid xs


applyInstruction1 :: Grid -> Instruction -> Grid
applyInstruction1 g (Toggle s e) = updateGrid g s e (\v -> 1-v)
applyInstruction1 g (On     s e) = updateGrid g s e (const 1)
applyInstruction1 g (Off    s e) = updateGrid g s e (const 0)
applyInstruction1 _ _ = error "Invalid instruction"


-- part 2
countBrightness :: Grid -> [String] -> Int
countBrightness g [] = sum $! M.elems g
countBrightness g (x:xs) = let inst = parseInstruction x
                               newGrid = applyInstruction2 g inst
                           in countBrightness newGrid xs


applyInstruction2 :: Grid -> Instruction -> Grid
applyInstruction2 g (Toggle (sx, sy) (ex, ey)) = M.fromList [((x,y), 2 + M.findWithDefault 0 (x,y) g)  | x <- [sx..ex], y <- [sy..ey]] `M.union` g
applyInstruction2 g (On     (sx, sy) (ex, ey)) = M.fromList [((x,y), 1 + M.findWithDefault 0 (x,y) g)  | x <- [sx..ex], y <- [sy..ey]] `M.union` g
applyInstruction2 g (Off    (sx, sy) (ex, ey)) = M.fromList [((x,y), max 0 (M.findWithDefault 0 (x,y) g - 1)) | x <- [sx..ex], y <- [sy..ey]] `M.union` g
applyInstruction2 _ _ = error "Invalid instruction"


part1 :: [String] -> String
part1 input = show $ countOn (grid 1000 1000) input

part2 :: [String] -> String
part2 input = show $ countBrightness (grid 1000 1000) input

module Main where

import           AOC.IO
import           Data.List (nub, permutations)
import qualified Data.Map.Strict as M (Map, fromList, findWithDefault)

type GainMap = M.Map (String, String) Int

main :: IO ()
main = do
  r1 <- processInput (totalGain [])
  putStrLn ("Part 1: " ++ show r1)
  r2 <- processInput (totalGain ["Me"])
  putStrLn ("Part 2: " ++ show r2)


totalGain :: [String] -> [String] -> Int
totalGain extra input = maximum $ map calculateGain arrangements where
  arrangements          = permutations (guests ++ extra)
  guests                = nub $ map (head . words) input
  calculateGain setting = sum $ map sumGains (pairs setting ++ [(last setting, head setting)])
  sumGains (f, s)       = M.findWithDefault 0 (f, s) gains + M.findWithDefault 0 (s, f) gains
  gains                 = toGainMap input

toGainMap :: [String] -> GainMap
toGainMap input = M.fromList $ map (\l -> let w = words l
                                          in parseLine [head w, init (w !! 10), w !! 2, w !! 3]) input where
  parseLine [x, y, "gain", gain] = ((x, y), read gain :: Int)
  parseLine [x, y, "lose", gain] = ((x, y), -1 * read gain :: Int)
  parseLine _ = error "Invalid input"

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

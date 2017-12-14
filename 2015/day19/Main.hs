module Main where

import AOC.IO
import Data.List (isPrefixOf, findIndices, tails, nub, sortOn)
import qualified Data.Text as T (Text, splitOn, pack, unpack)

type Rule = (String, String)


main :: IO ()
main = do
  (str, rules) <- processText parse
  putStr "Part 1: "
  print $ (length . nub) (replacements rules str)
  putStr "Part 2: "
  print $ build rules str


parse :: [T.Text] -> (String, [Rule])
parse lns =
  let (x:_:xs) = reverse lns
      toPair line = toStrPair $ T.splitOn (T.pack " => ") line
      toStrPair [k, v] = (T.unpack k, T.unpack v)
      toStrPair _ = error "invalid input"
      rules = map toPair xs
  in (T.unpack x, rules)


replacements :: [Rule] -> String -> [String]
replacements rules molecule = concatMap findReplacements rules where
  strTails = tails molecule
  findReplacements (str, repl) = map replace indices where
    indices = findIndices (isPrefixOf str) strTails
    len = length str
    replace idx = take idx molecule ++
                  repl ++
                  drop (idx + len) molecule


build :: [Rule] -> String -> Int
build rules molecule = build' molecule 0 where
  lenSortDsc = reverse $ sortOn (length . snd) rules
  existingReplacements ss (_, r) = not . null $
                                     findIndices (isPrefixOf r) ss
  build' "e" n = n
  build' mol n = build' next (n+1) where
    strTails = tails mol
    (from, to) = head $ filter (existingReplacements strTails) lenSortDsc
    idx = head $ findIndices (isPrefixOf to) strTails
    next = take idx mol ++ from ++ drop (idx + length to) mol


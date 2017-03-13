{-# LANGUAGE OverloadedStrings #-}
module Main where

import AOC.IO
import Data.Text.Read
import Data.Array
import Control.Monad.State
import qualified Data.Text as T (Text, init, splitOn)

data Inst = HLF T.Text | TPL T.Text | INC T.Text |
            JMP Int | JIE T.Text Int | JIO T.Text Int
          deriving (Show)

type Program = Array Int Inst -- (pc, instruction)

type ProgramState = (Int, (Int, Int)) -- (pc, (a, b))

main :: IO ()
main = do
  program <- processText parse
  let rng = bounds program
  putStr "Part 1: "
  let part1 = (0, (0, 0))
  print $ evalState (run program rng) part1
  putStr "Part 2: "
  let part2 = (0, (1, 0))
  print $ evalState (run program rng) part2


parse :: [T.Text] -> Program
parse input = array (0, length instList - 1) assocList where
  assocList = zip [0..] instList
  instList  = map parseLine input
  parseLine :: T.Text -> Inst
  parseLine line = toInst $ T.splitOn " " line
  toInst ("hlf":x:_) = HLF x
  toInst ("tpl":x:_) = TPL x
  toInst ("inc":x:_) = INC x
  toInst ("jmp":x:_) = JMP (parseOffset x)
  toInst ("jie":x:o:_) = JIE (T.init x) (parseOffset o)
  toInst ("jio":x:o:_) = JIO (T.init x) (parseOffset o)
  toInst _ = error "Invalid input"

  parseOffset :: T.Text -> Int
  parseOffset x = parseOffset' (signed decimal x) where
    parseOffset' (Right (v, _)) = v
    parseOffset' _ = error "Could not parse offset"


run :: Program -> (Int, Int) -> State ProgramState Int -- = b
run p (i, e)
  | i > e = do
      (_, (_, b)) <- get
      return b
  | otherwise = do
      (a, b) <- get
      let s@(pc, _) = exec (a, b) (p ! i)
      put s
      run p (pc, e)

exec :: ProgramState -> Inst -> ProgramState
exec (pc, (a, b)) (HLF "a") = (pc+1, (a `quot` 2, b))
exec (pc, (a, b)) (HLF "b") = (pc+1, (a, b `quot` 2))
exec (pc, (a, b)) (TPL "a") = (pc+1, (a * 3, b))
exec (pc, (a, b)) (TPL "b") = (pc+1, (a, b * 3))
exec (pc, (a, b)) (INC "a") = (pc+1, (a+1, b))
exec (pc, (a, b)) (INC "b") = (pc+1, (a, b+1))
exec (pc, s) (JMP i) = (pc+i, s)
exec (pc, (1, b)) (JIO "a" i) = (pc+i, (1, b))
exec (pc, (a, b)) (JIO "a" _) = (pc+1, (a, b))
exec (pc, (a, 1)) (JIO "b" i) = (pc+i, (a, 1))
exec (pc, (a, b)) (JIO "b" _) = (pc+1, (a, b))
exec (pc, (a, b)) (JIE "a" i) = if even a then (pc+i, (a, b))
                                else (pc+1, (a, b))
exec (pc, (a, b)) (JIE "b" i) = if even b then (pc+i, (a, b))
                                else (pc+1, (a, b))
exec _ _ = error "Invalid instruction"

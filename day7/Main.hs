module Main where

import           AOC.IO
import           Data.Bits (shift, complement, (.&.), (.|.))
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M (Map, insert, fromList, lookup)

type WireMap = M.Map Wire Expr
type ResultMap = M.Map Wire Int
type Wire = String

data Expr = Value Int
   | Token Wire
   | LShift Wire Wire
   | RShift Wire Wire
   | And Wire Wire
   | Or Wire Wire
   | Not Wire deriving (Show, Eq)

main :: IO ()
main = do
  wiremap <- processInput parse
  putStr "Part 1 - a: "
  print (fst $ evaluate wiremap "a")
  putStr "Part 2 - a: "
  print (fst $ evaluate (M.insert "b" (Value 46065) wiremap) "a")


parse :: [String] -> WireMap
parse rules = M.fromList $ map (\line -> let chunks = splitOn " -> " line
                                             w = last chunks
                                             inst = parseRule $ head chunks
                                         in (w, inst)) rules

parseRule :: String -> Expr
parseRule txt = case splitOn " " txt of
                   [x] -> Token x
                   ["NOT", x]       -> Not x
                   [x, "OR", y]     -> Or x y
                   [x, "AND", y]    -> And x y
                   [x, "RSHIFT", y] -> RShift x y
                   [x, "LSHIFT", y] -> LShift x y
                   _ -> error "Invalid instruction"


evaluate :: WireMap -> Wire -> (Int, WireMap)
evaluate symap wire = case M.lookup wire symap of
                      Just (Token w)    -> let (v, symap') = evaluate symap w
                                           in (v, M.insert wire (Value v) symap')
                      Just (LShift x y) -> let (v1, symap')  = evaluate symap x
                                               (v2, symap'') = evaluate symap' y
                                               v = shift v1 v2
                                           in (v, M.insert wire (Value v) symap'')
                      Just (RShift x y) -> let (v1, symap')  = evaluate symap x
                                               (v2, symap'') = evaluate symap' y
                                               v = shift v1 (-v2)
                                           in (v, M.insert wire (Value v) symap'')
                      Just (And x y)    -> let (v1, symap') = evaluate symap x
                                               (v2, symap'') = evaluate symap' y
                                               v = v1 .&. v2
                                           in (v, M.insert wire (Value v) symap'')
                      Just (Or x y)     -> let (v1, symap') = evaluate symap x
                                               (v2, symap'') = evaluate symap' y
                                               v = v1 .|. v2
                                           in (v, M.insert wire (Value v) symap'')
                      Just (Not x)      -> let (v, symap') = evaluate symap x
                                               r = complement v
                                           in (r, M.insert wire (Value v) symap')
                      Just (Value x)    -> (x, symap)
                      Nothing           -> (read wire :: Int, symap)

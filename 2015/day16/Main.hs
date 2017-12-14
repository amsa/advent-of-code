module Main where

import AOC.IO
import Data.List (elemIndex)
import qualified Data.Map.Strict as M

type Aunt = M.Map String Int


defaultMap :: Aunt
defaultMap =
  M.fromList [("children", -1 :: Int), ("cats", -1), ("samoyeds", -1),
              ("pomeranians", -1), ("akitas", -1), ("vizslas", -1),
              ("goldfish", -1), ("trees", -1), ("cars", -1), ("perfumes", -1)]

main :: IO ()
main = do
  aunts <- processInput parseAuntInfo
  let giftFrom = M.fromList [("children", 3 :: Int), ("cats", 7), ("samoyeds", 2),
                  ("pomeranians", 3), ("akitas", 0), ("vizslas", 0),
                  ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]
  putStrLn "Part 1: "
  print $ findSue giftFrom aunts (\_ v1 v2 -> v1 == v2)
  putStrLn "Part 2: "
  print $ findSue giftFrom aunts compareRange where
    compareRange "trees"       vg v = vg < v
    compareRange "cats"        vg v = vg < v
    compareRange "pomeranians" vg v = vg > v
    compareRange "goldfish"    vg v = vg > v
    compareRange _             vg v = vg == v

parseAuntInfo :: [String]
              -> [Aunt]
parseAuntInfo line = M.empty:map parse line where -- add an empty one for index 0
  parse ln =
    let assocList = toAssocList (dropWhile (/= ':') ln)
        auntInfo = M.fromList . toInt $ assocList
        toInt [] = []
        toInt ((k,v):xs) = (k,read v :: Int):toInt xs
    in auntInfo

findSue :: Aunt
        -> [Aunt]
        -> (String -> Int -> Int -> Bool)
        -> Maybe Int
findSue sue aunts compareFn = last candidate `elemIndex` aunts where
  candidate = filter matched aunts
  matched :: Aunt -> Bool
  matched x = all (==True) values where
    values = M.elems $ M.mapWithKey compareKeys x
    compareKeys :: String -> Int -> Bool
    compareKeys k v = case M.lookup k sue of
                        Just vg -> compareFn k vg v
                        Nothing -> False

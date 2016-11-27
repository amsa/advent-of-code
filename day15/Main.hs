module Main where

import AOC.IO

data Ingredient = Ingredient {
  name :: String,
  capacity :: Int,
  durability :: Int,
  flavor :: Int,
  texture :: Int,
  calories :: Int
  } deriving (Show, Eq)


main :: IO ()
main = do
  r <- processInput ingredients
  putStrLn "Part 1: " >> print (maximizeScore r)
  putStrLn "Part 2: " >> print (maximize500Cal r)

ingredients :: [String]
            -> [Ingredient]
ingredients = map (\line -> let l = words line
                         in Ingredient { name        = init $ head l, -- skip colon e.g. Sugar:
                                         capacity    = read (init $ l !! 2)  :: Int, -- skip comma e.g. 10,
                                         durability  = read (init $ l !! 4)  :: Int,
                                         flavor      = read (init $ l !! 6)  :: Int,
                                         texture     = read (init $ l !! 8)  :: Int,
                                         calories    = read (l !! 10)  :: Int -- last value (no comma)
                                       })

calculateScores :: [Ingredient] -> [[Int]]
calculateScores ingred = scores
  where weights  = [zip [w1, w2, w3, 100 - w1 - w2 - w3] ingred |
                  w1 <- [0..100],
                  w2 <- [0..100],
                  w3 <- [0..100],
                  100 - w1 - w2 - w3 >= 0]
        getScore = foldr (\(w, x) [cap, dur, fla, tex, cal] -> [
                                     w * capacity x + cap,
                                     w * durability x + dur,
                                     w * flavor x + fla,
                                     w * texture x + tex,
                                     w * calories x + cal
                                    ]) [0, 0, 0, 0, 0]
        scores = map getScore weights

findMax :: [[Int]] -> Int
findMax scores = maximum $ map (\x -> let ing = init x -- exclude calories
                                      in if any (<= 0) ing then 0
                                         else product ing) scores

-- part 1: find best recipe
maximizeScore :: [Ingredient] -> Int
maximizeScore = findMax . calculateScores

-- part 2: find best recipe with 500 calories per cookie
maximize500Cal :: [Ingredient] -> Int
maximize500Cal ingred =
  let scores = calculateScores ingred
      valid  = filter (\x -> last x == 500) scores
  in findMax valid

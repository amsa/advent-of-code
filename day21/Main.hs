module Main where

import Data.List (subsequences)

data Item = Item {
  name   :: String,
  cost   :: Int,
  damage :: Int,
  armor  :: Int
  } deriving (Show)

-- (Points, (Damage, Armor))
type Player = (Int, (Int, Int))

weapons :: [Item]
weapons = [
  Item { name = "Dagger"     , cost = 8,  damage = 4, armor = 0 },
  Item { name = "Shortsword" , cost = 10, damage = 5, armor = 0 },
  Item { name = "Warhammer"  , cost = 25, damage = 6, armor = 0 },
  Item { name = "Longsword"  , cost = 40, damage = 7, armor = 0 },
  Item { name = "Greataxe"   , cost = 74, damage = 8, armor = 0 }
  ]

armors :: [Item]
armors = [
  Item { name = "Leather"   , cost = 13, damage = 0, armor = 1 },
  Item { name = "Chainmail" , cost = 31, damage = 0, armor = 2 },
  Item { name = "Splintmail", cost = 53, damage = 0, armor = 3 },
  Item { name = "Bandedmail", cost = 75, damage = 0, armor = 4 },
  Item { name = "Platemail" , cost = 102, damage = 0, armor = 5 }
  ]

rings :: [Item]
rings = [
  Item { name = "Damage +1",  cost = 25,  damage = 1, armor = 0 },
  Item { name = "Damage +2",  cost = 50,  damage = 2, armor = 0 },
  Item { name = "Damage +3",  cost = 100, damage = 3, armor = 0 },
  Item { name = "Defense +1", cost = 20,  damage = 0, armor = 1 },
  Item { name = "Defense +1", cost = 40,  damage = 0, armor = 2 },
  Item { name = "Defense +1", cost = 80,  damage = 0, armor = 3 }
  ]

main :: IO ()
main = do
  let boss = (103, (9, 2))
  let playerPoints = 100
  putStr "Part 1: "
  print $ part1 boss playerPoints
  putStr "Part 2: "
  print $ part2 boss playerPoints


equip :: [[Item]]
equip = [[w] ++ a ++ r | w <- weapons, a <- armr, r <- rngs] where
  -- up to 1 armor
  armr = filter (\x -> length x <= 1) (subsequences armors)
  -- up to 2 rings
  rngs = filter (\x -> length x <= 2) (subsequences rings)

play :: Player -> Player -> Bool
play (p1, (d1, a1)) (p2, (d2, a2))
  | p1 < 0  && p2 < 0  = p1 > p2
  | p1 > 0  && p2 <= 0 = True
  | p1 <= 0 && p2 > 0  = False
  | otherwise          =
    let deduct p v = p - maximum [v, 1]
        newP2 = deduct p2 (d1-a2)
        newP1 = deduct p1 (d2-a1)
    in ((newP2 <= 0) ||
        ((newP1 > 0) && play (newP1, (d1, a1)) (newP2, (d2, a2))))

part1 :: Player -> Int -> Int
part1 boss playerPoints = findMin where
  findMin = minimum . map snd $ filter fst allGames
  allGames = map (determineWinner boss playerPoints) equip

part2 :: Player -> Int -> Int
part2 boss playerPoints = findMax where
  findMax  = maximum . map snd $ filter (not . fst) allGames
  allGames = map (determineWinner boss playerPoints) equip

determineWinner ::
  Player -> Int -> [Item] -> (Bool, Int) -- (winner/loser, cost)
determineWinner boss playerPoints items =
  (play (playerPoints, (dmg, arm)) boss, cst) where
  dmg = sum $ map damage items
  arm = sum $ map armor items
  cst = sum $ map cost items


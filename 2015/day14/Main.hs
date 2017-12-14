module Main where

import AOC.IO

data ScoreBoard = ScoreBoard {
  score :: Int,
  position :: Int,
  runTimer :: Int,
  restTimer :: Int
  } deriving (Show)

data Rendeer = Rendeer {
  name :: String,
  speed :: Int,
  duration :: Int,
  rest :: Int
  } deriving (Show, Eq)


main :: IO ()
main = do
  r <- processInput rendeers
  putStrLn $ "Part 1: " ++ show (findWinner r 2503)
  putStrLn $ "Part 2: " ++ show (countScore r 2503)

rendeers :: [String]
         -> [Rendeer]
rendeers = map (\line -> let l = words line
                         in Rendeer { name     = head l,
                                      speed    = read (l !! 3)  :: Int,
                                      duration = read (l !! 6)  :: Int,
                                      rest     = read (l !! 13) :: Int
                                    })

-- part 1: find out how far the winning rendeer has traveled
findWinner :: [Rendeer] -> Int
           -> Int
findWinner ren sec = maximum . map (findWinner' 0 sec) $ ren where
  findWinner' :: Int -> Int -> Rendeer -> Int
  findWinner' acc seconds r
    | seconds <= 0 = acc
    | otherwise =
      let d = duration r
          step = minimum [seconds, d] * speed r
      in findWinner' (acc+step) (seconds - d - rest r) r

countScore :: [Rendeer] -> Int -> Int
countScore rens = countScore' initScores where
  initScores = map (\x -> (x, ScoreBoard {position=0, score=0, runTimer=0, restTimer=0})) rens

  maxBy :: (ScoreBoard -> Int) -> [(a, ScoreBoard)] -> Int
  maxBy fn = maximum . map (fn . snd)

  countScore' :: [(Rendeer, ScoreBoard)] -> Int -> Int
  countScore' r 0 = maxBy score r
  countScore' r timer =
    let next = map step r
        lead = maxBy position next
        bumpScores (ren, sb) = if position sb == lead
                              then (ren, sb {score = score sb + 1})
                              else (ren, sb)
        newState = map bumpScores next
    in countScore' newState (timer-1)

  step (ren, sb@ScoreBoard {position=p, restTimer=0, runTimer=0}) = (ren, sb {
    position  = p + speed ren,
    runTimer  = duration ren - 1,
    restTimer = rest ren})
  step (ren, sb@ScoreBoard {runTimer=0, restTimer=rst}) = (ren, sb {restTimer = rst - 1})
  step (ren, sb@ScoreBoard {position=p, runTimer=run}) = (ren, sb {
    position  = p + speed ren,
    runTimer  = run - 1})

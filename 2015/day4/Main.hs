module Main where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as Hex

main :: IO ()
main = do
  putStrLn "Part 1:"
  print $ findKey "bgvyzdsv" 5
  putStrLn "Part 2:"
  print $ findKey "bgvyzdsv" 6

md5hash :: String -> String
md5hash = C.unpack . Hex.encode . MD5.hash . C.pack

findKey :: String -> Int -> Int
findKey prefix n =
  let h x = md5hash (prefix ++ show x)
      topN y = take n (h y)
  in head [x | x <- [0..], all (== '0') (topN x)]

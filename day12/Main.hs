module Main where

import AOC.IO
import Numeric
import Data.Aeson
import Data.Scientific
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as B (ByteString, readFile)
import qualified Data.HashMap.Strict as HM (toList)
import qualified Data.Vector as V (toList)
import qualified Data.Text as T


main :: IO ()
main = do
  r1 <- processInput part1
  putStrLn $ "Part 1: " ++ show r1
  r2 <- processInput' B.readFile part2
  putStrLn $ "Part 2: " ++ show r2


part1 :: [String] -> Float
part1 = sumNumbers where
  jsonParser :: GenParser Char st [[String]]
  jsonParser = line `endBy` eol
  line :: GenParser Char st [String]
  line = number `sepBy` noneOf (digits ++ "\n")
  number :: GenParser Char st String
  number = many (oneOf digits)
  digits :: String
  digits  = "-0123456789"
  eol :: GenParser Char st Char
  eol = char '\n'
  sumNumbers :: [String] -> Float
  sumNumbers input = let res = parse jsonParser "error" (unlines input)
    in case res of
      Left _ -> error "parse failed"
      Right r -> toNum lst where
        lst = (filter (not . null) . concat) r
        toNum = foldr (\e acc -> case readSigned readFloat e of
                                            [(num, _)] -> acc + num
                                            _ -> acc ) 0


part2 :: B.ByteString -> Integer
part2 input = sumNumbers jsonParser where
  jsonParser = decode input :: Maybe Value
  sumNumbers (Just a) = sumNumbers' 0 [a]
  sumNumbers Nothing = 0

  sumNumbers' :: Integer -> [Value] -> Integer
  sumNumbers' acc [] = acc
  sumNumbers' acc (Object x:xs) = if hasRed
                                  then sumNumbers' acc xs
                                  else sumNumbers' acc (values ++ xs) where
                    (_, values) = unzip (HM.toList x)
                    hasRed = (String $ T.pack "red") `elem` values
  sumNumbers' acc (Array x:xs)  = sumNumbers' acc (V.toList x ++ xs)
  sumNumbers' acc (Number x:xs) = sumNumbers' (coefficient x * 10 ^ base10Exponent x + acc) xs
  sumNumbers' acc (_:xs) = sumNumbers' acc xs

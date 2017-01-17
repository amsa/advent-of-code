module AOC.IO where

import           Control.Exception (catch, IOException)
import           System.Environment (getArgs)
import           System.IO (hPutStrLn, stderr)
import qualified Data.Text as T (Text, lines)
import qualified Data.Text.IO as T (readFile)


processInput :: ([String] -> a) -> IO a
processInput fn = processInput' readFile (fn . lines)

processText :: ([T.Text] -> a) -> IO a
processText fn = processInput' T.readFile (fn . T.lines)

processInput' :: (String -> IO a) -> (a -> b) -> IO b
processInput' readerFn fn = do
  args <- getArgs
  case args of
    (file:_) -> do
      contents <- catch (readerFn file) (\e -> do
                                            let err = show (e :: IOException)
                                            hPutStrLn stderr ("Error while openning file: " ++ err)
                                            error "")
      return $ fn contents
    _ -> error "Invalid number of arguments."


eachLine :: Show a1 => (a -> a1) -> [a] -> [String]
eachLine lineFn = map $ show . lineFn

toAssocList :: String
         -> [(String, String)]
toAssocList = _toTuple . _parseMap "" where
  _parseMap :: String -> String -> [String]
  _parseMap ""  [] = []
  _parseMap tmp [] = reverse tmp:_parseMap "" []
  _parseMap tmp (' ':xs) = _parseMap tmp xs
  _parseMap ""  (':':xs) = _parseMap "" xs
  _parseMap tmp (':':xs) = reverse tmp:_parseMap "" xs
  _parseMap tmp (',':xs) = reverse tmp:_parseMap "" xs
  _parseMap tmp (x:xs)   = _parseMap (x:tmp) xs

  _toTuple :: [a] -> [(a, a)]
  _toTuple [] = []
  _toTuple (k:v:vs) = (k,v):_toTuple vs
  _toTuple _ = []

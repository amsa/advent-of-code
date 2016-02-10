module AOC.IO where

import           Control.Exception (catch, IOException)
import           System.Environment (getArgs)
import           System.IO (hPutStrLn, stderr)

processInput :: ([String] -> b) -> IO b
processInput fn = processInput' readFile (fn . lines)

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


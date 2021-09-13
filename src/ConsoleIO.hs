module ConsoleIO where

class Monad m => ConsoleIO m where
  writeLine :: String -> m ()
  readLine :: m String

instance ConsoleIO IO where
  writeLine = putStrLn
  readLine = getLine

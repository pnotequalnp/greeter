module Greeter where

import ConsoleIO
import Text.Read (readMaybe)

greet :: ConsoleIO m => m ()
greet = do
  name <- getName
  age <- ensureAge
  writeLine $ "Hello " <> name <> "! You are " <> oldness age <> "!"

getName :: ConsoleIO m => m String
getName = do
  writeLine "Please enter your name:"
  readLine

getAge :: ConsoleIO m => m (Maybe Int)
getAge = do
  writeLine "Please enter your age:"
  readMaybe <$> readLine

ensureAge :: ConsoleIO m => m Int
ensureAge =
  getAge >>= \case
    Nothing -> ensureAge
    Just age -> pure age

oldness :: Int -> String
oldness age
  | age < 18 = "very young"
  | age < 35 = "young"
  | age < 65 = "old"
  | otherwise = "very old"

module PureShell.Cat.Cat where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Effect.Aff (Aff, Error, try)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)

data CatErrors
  = FileNotReadable FilePath Error
  | FileNotExists FilePath

instance showCatErrors :: Show CatErrors where
  show (FileNotReadable path err) = "Can't read file \"" <> path <> "\" with an error => " <> show err
  show (FileNotExists path) = "Oops bro. FilePath \"" <> path <> "\" doesn't exist"

cat :: FilePath -> Aff Unit
cat filePath = do
  fileExists <- FS.exists filePath
  if fileExists
  then do
    result <- try $ FS.readTextFile UTF8 filePath
    liftEffect $ case result of
      Left err -> logShow (FileNotReadable filePath err)
      Right res -> logShow res
  else liftEffect $ logShow (FileNotExists filePath)

-- | Handles the remaining arguments from `Main` function. Extract the first
-- | element then pass to `cat`
handleCatArgs :: List String -> Aff Unit
handleCatArgs Nil = liftEffect $ logShow "You must provide a file path"
handleCatArgs (f : _) = cat f

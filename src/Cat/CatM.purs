module PureShell.Cat.CatM where

import Prelude

import Control.Monad.Error.Class (class MonadError, try)
import Control.Monad.Logger.Class (class MonadLogger, error, info)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.List (List(..), (:))
import Data.Map (empty)
import Node.Encoding (Encoding(..))
import Node.Path (FilePath)

data CatErrors e
  = FileNotReadable FilePath e
  | FileNotExists FilePath

instance showCatErrors :: Show e => Show (CatErrors e) where
  show (FileNotReadable path err) = "Can't read file \"" <> path <> "\" with an error => " <> show err
  show (FileNotExists path) = "Oops bro. FilePath \"" <> path <> "\" doesn't exist"

-- | A typeclass to encode all effects related to File System operations
-- | that are being used in `cat`. Later on, the top level monad (i.e `AppM`)
-- | should implement this typeclass to be able to call `cat`
class (Monad m) <= MonadFile m where
  exists :: FilePath -> m Boolean
  readTextFile :: Encoding -> FilePath -> m String

-- | Nice little technique to emulate Constraint Kinds in Purescript
-- | @see https://github.com/JordanMartinez/purescript-jordans-reference/blob/latestRelease/31-Design-Patterns/07-Simulating-Constraint-Kinds.md
type MonadCat m e r =
  Show e =>
  MonadFile m =>
  MonadError e m =>
  r

-- | The pure version of `cat` by using Constraints
cat :: forall m e. MonadCat m e (FilePath -> m (Either (CatErrors e) String))
cat filePath = do
  fileExists <- exists filePath
  if fileExists
  then do
    result <- try $ readTextFile UTF8 filePath
    pure $ bimap (FileNotReadable filePath) identity result
  else pure $ Left (FileNotExists filePath :: CatErrors e)

-- | Handles the remaining arguments from `Main` function. Extract the first
-- | element then pass to `cat` Otherwise, yield some error
handleCatArgs :: forall m e. MonadLogger m => MonadCat m e (List String -> m Unit)
handleCatArgs Nil = error' "You must provide a file path"
handleCatArgs (f : _) = (cat f) >>= either error' info'

error' :: forall m a. MonadLogger m => Show a => a -> m Unit
error' a = error empty $ show a

info' :: forall m a. MonadLogger m => Show a => a -> m Unit
info' a = info empty $ show a

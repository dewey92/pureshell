module PureShell.Cat.Cat
  ( CatErrors(..)
  , cat
  ) where

import Prelude

import Control.Monad.Except (ExceptT, except, withExceptT)
import Data.Either (Either(..))
import Node.Path (FilePath)
import PureShell.Common.MonadFS (class MonadFS, exists, readFile)

data CatErrors
  = FileNotExists FilePath
  | FileIsDir FilePath
  | MiscError FilePath

derive instance eqCatErrors :: Eq CatErrors

instance showCatErrors :: Show CatErrors where
  show (FileNotExists path) = "Oops bro. FilePath \"" <> path <> "\" doesn't exist"
  show (FileIsDir path) = path <> " is a directory"
  show (MiscError path) = "Can't read \"" <> path <> "\""

-- | The pure version of `cat` by using Constraints
cat :: ∀ m e. MonadFS e m => FilePath -> ExceptT CatErrors m String
cat filePath = do
  doesExist <- withExceptT (const (MiscError filePath)) (exists filePath)
  case doesExist of
    false -> except $ Left (FileNotExists filePath)
    true -> withExceptT (const (MiscError filePath)) (readFile filePath)

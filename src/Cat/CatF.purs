module PureShell.Cat.CaF
  ( CatErrors(..)
  , CatF
  , cat
  ) where

import Prelude

import Control.Monad.Free (Free, liftF, runFree, runFreeM)
import Data.Either (Either(..), either)
import Effect.Aff (Aff, Error, try)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)

-- | All errors expected to occur in the program
-- |
-- | TODO: add `FilePathIsDir` error
data CatErrors e
  = FileNotReadable FilePath e
  | FileNotExists FilePath

instance showCatErrors :: Show e => Show (CatErrors e) where
  show (FileNotReadable path err) = "Can't read file \"" <> path <> "\" with an error => " <> show err
  show (FileNotExists path) = "Oops bro. FilePath \"" <> path <> "\" doesn't exist"

-- | The algebra of `cat`. We list all effectful operations needed to be run in this ADT.
-- | `Return` is to terminate the program and pass the value to the caller
data CatF e a
  = Exists FilePath (Boolean -> a)
  | ReadFile FilePath (Either e String -> a)
  | Return a

-- | Free needs its algebra to be a functor.
-- | Or more precisely that which has a Kind * -> *
derive instance catFunctor :: Functor (CatF e)

type Cat e a = Free (CatF e) a

exists :: forall e. FilePath -> Cat e Boolean
exists fp = liftF $ Exists fp identity

readFile :: forall e. FilePath -> Cat e (Either e String)
readFile fp = liftF $ ReadFile fp identity

return :: forall e. String -> Cat e String
return = liftF <<< Return

-- | The pure version of `cat` by using Free Monad. What it means by using Free
-- | is that functions like `exists` and `readFile` are actualluy returning
-- | **descriptions** of a program. Thus making the whole operations _free_ (oops)
-- | of side effects.
cat :: forall e. Show e => FilePath -> Cat e String
cat filePath = do
  fileExists <- exists filePath
  if fileExists
  then do
    result <- readFile filePath
    return $ either (FileNotReadable filePath >>> show) identity result
  else return $ show (FileNotExists filePath :: CatErrors e)

-- | The interpreter of `cat`. If `cat` is the "what" of a program, then
-- | this is the "how" of the program: how is the program going to be run?
-- | Is it using real file system? Or using remote file system?
-- |
-- | a.k.a the implementation details
runCat :: Cat Error String -> Aff String
runCat = runFreeM go
  where
    go (Exists fp next) = do
      fileExists <- FS.exists fp
      pure $ next fileExists
    go (ReadFile fp next) = do
      content <- try $ FS.readTextFile UTF8 fp
      pure $ next content
    go (Return a) = pure a

-- | Another interpreter of the `cat` program. In this case, an interpreter
-- | for unit testing
runCatTest :: Cat String String -> String
runCatTest = runFree go
  where
    go (Exists fp next)
      | fp == "index.html" = next true
      | otherwise = next false

    go (ReadFile fp next)
      | fp == "index.html" = next (Right "<h1>File content</h1>")
      | otherwise = next $ Left ("Oops. " <> fp <> "is unreadable")

    go (Return a) = a

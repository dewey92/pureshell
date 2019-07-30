module PureShell.Cat.CatSpec (spec) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Data.List (List(..))
import Effect.Aff (Aff, Error, error)
import PureShell.Cat.Cat (CatErrors(..), cat)
import PureShell.Common.MonadFS (class MonadFS)
import PureShell.Mock.MonadFS (mockMetadata)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

newtype TestCatM a = TestCatM (Aff a)

runTestCatM :: TestCatM ~> Aff
runTestCatM (TestCatM a) = a

derive newtype instance functorTestCatM :: Functor TestCatM
derive newtype instance applyTestCatM :: Apply TestCatM
derive newtype instance applicativeTestCatM :: Applicative TestCatM
derive newtype instance bindTestCatM :: Bind TestCatM
derive newtype instance monadTestCatM :: Monad TestCatM
-- | Error handling
derive newtype instance monadThrowTestCatM :: MonadThrow Error TestCatM
derive newtype instance monadErrorTestCatM :: MonadError Error TestCatM

instance monadFSTestCatM :: MonadFS Error TestCatM where
  exists fp
    | fp == "validFile" || fp == "someDir" = pure true
    | otherwise = pure false
  readFile fp
    | fp == "validFile" = pure "file content"
    | otherwise = throwError $ error "a dir"
  readDir _ = pure Nil
  getMetadata _ = pure mockMetadata

-- spec :: ∀ m e. Monad m => Show e => MonadFS Error m => SpecT m Unit m Unit
spec :: Spec Unit
spec = describe "cat" do
  it "throws when reading an invalid path" do
    let invalidPath = "invalid/path"
    errMsg <- runTestCatM $ cat invalidPath
    errMsg `shouldEqual` show (FileNotExists invalidPath :: CatErrors Error)

  it "throws when a directory is passed" do
    let dirPath = "someDir"
    errMsg <- runTestCatM $ cat dirPath
    errMsg `shouldContain` "Can\'t read file" -- show (FileNotReadable dirPath "Error: a dir")

  it "succesfully reads the file" do
    res <- runTestCatM $ cat "validFile"
    res `shouldEqual` "file content"

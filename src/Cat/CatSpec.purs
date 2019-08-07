module PureShell.Cat.CatSpec (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import PureShell.Cat.Cat (CatErrors(..), cat)
import PureShell.Mock.MonadFS (MonadFSIns(..), baseInst, runTestM)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

{- newtype TestCatM a = TestCatM (Aff a)

runTest :: ∀ e r. ExceptT e TestCatM r -> Aff (Either e r)
runTest (ExceptT (TestCatM a)) = a

derive newtype instance functorTestCatM :: Functor TestCatM
derive newtype instance applyTestCatM :: Apply TestCatM
derive newtype instance applicativeTestCatM :: Applicative TestCatM
derive newtype instance bindTestCatM :: Bind TestCatM
derive newtype instance monadTestCatM :: Monad TestCatM

instance monadFSTestCatM :: MonadFS Error TestCatM where
  exists fp
    | fp == "validFile" || fp == "someDir" = pure true
    | otherwise = pure false
  readFile fp
    | fp == "validFile" = pure "file content"
    | otherwise = throwError $ error "a dir"
  readDir _ = pure Nil
  getMetadata _ = pure mockMetadata
  rename _ _ = pure unit -}

spec :: Spec Unit
spec = describe "cat" do
  it "throws when reading an invalid path" do
    let invalidPath = "invalid/path"
    let inst = MonadFSIns $ baseInst { _exists = \_ -> pure false }
    err <- runTestM inst $ cat invalidPath
    err `shouldEqual` (Left $ FileNotExists invalidPath)

  it "throws when a directory is passed" do
    let dirPath = "someDir"
    let inst = MonadFSIns $ baseInst {
      _exists = \_ -> pure true,
      _readFile = \_ -> throwError $ MiscError dirPath
    }
    err <- runTestM inst $ cat dirPath
    err `shouldEqual` (Left $ MiscError dirPath)

  it "succesfully reads the file" do
    let inst = MonadFSIns $ baseInst {
      _exists = \_ -> pure true,
      _readFile = \_ -> pure "file content"
    }
    res <- runTestM inst $ cat "validFile"
    res `shouldEqual` (Right $ "file content")

module PureShell.Cat.CatSpec (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import PureShell.Cat.Cat (CatErrors(..), cat)
import PureShell.Mock.MonadFS (MonadFSIns(..), baseInst, runTestM)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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

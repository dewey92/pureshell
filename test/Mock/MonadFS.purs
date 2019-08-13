module PureShell.Mock.MonadFS where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (class MonadAsk, ReaderT(..), ask)
import Data.Either (Either)
import Data.Function.Uncurried (mkFn0)
import Data.JSDate (fromTime)
import Data.List (List, singleton)
import Effect.Aff (Aff)
import Node.FS.Stats (Stats(..))
import Node.Path (FilePath)
import PureShell.Common.MonadFS (class MonadFS)

-- | A good read on how to substitute instances with a lot of flexibility
-- | @see https://lexi-lambda.github.io/blog/2016/10/03/using-types-to-unit-test-in-haskell/
type MockFS m e = {
  _exists :: FilePath -> ExceptT e m Boolean,
  _readFile :: FilePath -> ExceptT e m String,
  _readDir :: FilePath -> ExceptT e m (List String)
}
data MonadFSIns m e = MonadFSIns (MockFS m e)

newtype TestM e m a = TestM (ReaderT (MonadFSIns (TestM e m) e) Aff a)

runTestM :: ∀ m err res
  .  MonadFSIns (TestM err m) err
  -> ExceptT err (TestM err m) res
  -> Aff (Either err res)
runTestM env (ExceptT (TestM (ReaderT t))) = t env >>= pure

derive newtype instance functorTestM :: Functor (TestM e m)
derive newtype instance applyTestM :: Apply (TestM e m)
derive newtype instance applicativeTestM :: Applicative (TestM e m)
derive newtype instance bindTestM :: Bind (TestM e m)
derive newtype instance monadTestM :: Monad (TestM e m)

derive newtype instance monadReaderTestM :: MonadAsk (MonadFSIns (TestM e m) e) (TestM e m)

instance monadFSTestM :: MonadFS e (TestM e m) where
  exists path = do
    MonadFSIns { _exists } <- ask
    _exists path
  readFile path = do
    MonadFSIns { _readFile } <- ask
    _readFile path
  readDir path = do
    MonadFSIns { _readDir } <- ask
    _readDir path
  getMetadata _ = pure mockMetadata
  rename _ _ = pure unit

baseInst :: ∀ m e. Monad m => MockFS m e
baseInst = {
  _exists: \_ -> pure true,
  _readFile: \_ -> pure "readFile unimplemented",
  _readDir: \_ -> pure $ singleton "readDir unimplemented"
}

mockMetadata :: Stats
mockMetadata = Stats { dev: 0.0
  , mode: 0.0
  , nlink: 0.0
  , uid: 0.0
  , gid: 0.0
  , rdev: 0.0
  , ino: 0.0
  , size: 0.0
  , atime: fromTime 0.0
  , mtime: fromTime 0.0
  , ctime: fromTime 0.0
  , isFile: mkFn0 \_ -> true
  , isDirectory: mkFn0 \_ -> false
  , isBlockDevice: mkFn0 \_ -> true
  , isCharacterDevice: mkFn0 \_ -> true
  , isFIFO: mkFn0 \_ -> true
  , isSocket: mkFn0 \_ -> false
  }
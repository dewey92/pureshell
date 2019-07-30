module PureShell.Mock.MonadFS where

import Data.Function.Uncurried (mkFn0)
import Data.JSDate (fromTime)
import Node.FS.Stats (Stats(..))

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
module PureShell.Common.MonadFS
  ( class MonadFS
  , exists
  , readFile
  , readDir
  , getMetadata
  , module ME
  ) where

import Prelude

import Control.Monad.Error.Class (try) as ME
import Control.Monad.Except (ExceptT)
import Data.List (List)
import Node.FS.Stats (Stats) as NodeFs
import Node.Path (FilePath)

-- | Define a monad for file system operations
class Monad m <= MonadFS e m | m -> e where
  -- checking
  exists :: FilePath -> ExceptT e m Boolean

  -- reading files or dirs
  readFile :: FilePath -> ExceptT e m String
  readDir :: FilePath -> ExceptT e m (List FilePath)
  getMetadata :: FilePath -> ExceptT e m NodeFs.Stats

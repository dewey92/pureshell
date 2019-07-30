module PureShell.Common.MonadFS
  ( class MonadFS
  , exists
  , readFile
  , readDir
  , getMetadata
  , module ME
  ) where


import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Error.Class (try) as ME
import Data.List (List)
import Node.FS.Stats (Stats) as NodeFs
import Node.Path (FilePath)

-- | Define a monad for file system operations
class MonadError e m <= MonadFS e m | m -> e where
  -- checking
  exists :: FilePath -> m Boolean

  -- reading files or dirs
  readFile :: FilePath -> m String
  readDir :: FilePath -> m (List FilePath)
  getMetadata :: FilePath -> m NodeFs.Stats

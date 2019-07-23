module PureShell.Common.FileM
  ( class MonadFs
  , exists
  , readTextFile
  , readDir
  , getMetadata
  , module ME
  ) where


import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Error.Class (try) as ME
import Data.List (List)
import Node.Encoding (Encoding)
import Node.FS.Stats (Stats) as NodeFs
import Node.Path (FilePath)

-- | Define a monad for file system operations. All Node.FS operations originally
-- | returning `Array` will be replaced by `List` for richer features. Please
-- | note also that I intentionally rename some functions from `Node.FS`
-- | just to make them nicer
class MonadError e m <= MonadFs e m | m -> e where
  -- checking
  exists :: FilePath -> m Boolean

  -- reading files or dirs
  readTextFile :: Encoding -> FilePath -> m String
  readDir :: FilePath -> m (List FilePath)
  getMetadata :: FilePath -> m NodeFs.Stats

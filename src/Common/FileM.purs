module PureShell.Common.FileM
  ( class MonadFs
  , exists
  , readTextFile
  , readDir
  , getMetadata
  , toRelativeOrAbsPath
  , toActualName
  , Visibility (..)
  , FileSystemType(..)
  , toFileSystemType
  , isFile
  , isHidden
  , isDirectory
  , module ME
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Error.Class (try) as ME
import Data.Array (last)
import Data.List (List)
import Data.Maybe (Maybe, fromJust)
import Data.String (Pattern(..), split)
import Node.Encoding (Encoding)
import Node.FS.Stats (Stats, isDirectory) as NodeFs
import Node.Path (FilePath)
import Options.Applicative.Internal.Utils (startsWith)
import Partial.Unsafe (unsafePartial)

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

-- Some helper functions to deal with file paths. Ideally there should be some kind of
-- `newtype` just to differentiate between Relative, Absoulte, and Actual Name of a filepath.
-- But that requires extra work and not sure if it's worth the effort

toRelativeOrAbsPath :: FilePath -> FilePath -> FilePath
toRelativeOrAbsPath prefix fp = prefix <> "/" <> fp

toActualName :: FilePath -> FilePath
toActualName fp = split (Pattern "/") fp
  # last
  # (\s -> unsafePartial $ cast s)
  where
    cast :: Partial => Maybe FilePath -> FilePath
    cast = fromJust

data Visibility = Hidden | Visible

identifyVisibility :: FilePath -> Visibility
identifyVisibility filePath
  | startsWith (Pattern ".") (toActualName filePath) = Hidden
  | otherwise = Visible

-- | Handling file system is a bit tricky since sometimes we have to deal with
-- | different types of `filepath` with different operations as well. Generally,
-- | those files can be categorized into `File` and `Directory` along with the visibility

data FileSystemType
  = File FilePath Visibility | Directory FilePath Visibility

instance showFileSystemType :: Show FileSystemType where
  show (File f _) = f
  show (Directory d _) = d

isFile :: FileSystemType -> Boolean
isFile (File _ _) = true
isFile _ = false

isHidden :: FileSystemType -> Boolean
isHidden (File _ Hidden) = true
isHidden (Directory _ Hidden) = true
isHidden _ = false

isDirectory :: FileSystemType -> Boolean
isDirectory (Directory _ _) = true
isDirectory _ = false

-- | Then define some behaviour to convert arbitrary filepaths to our data type
-- |
-- | A `Directory` is whatever filepath that's not a file
-- | A `HiddenFile` is a file that's prefixed by `.`
--  | Otehrwise it's just a normal file
toFileSystemType :: FilePath -> NodeFs.Stats -> FileSystemType
toFileSystemType fp s
  | NodeFs.isDirectory s = Directory (toActualName fp) (identifyVisibility fp)
  | otherwise = File (toActualName fp) (identifyVisibility fp)

module PureShell.Ls.Types
  ( LsOptions
  , prefixWith
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

import Control.Monad.Error.Class (try) as ME
import Data.Array (last)
import Data.Maybe (Maybe, fromJust)
import Data.String (Pattern(..), split)
import Node.FS.Stats (Stats, isDirectory) as NodeFs
import Node.Path (FilePath)
import Options.Applicative.Internal.Utils (startsWith)
import Partial.Unsafe (unsafePartial)

-- | These options are actually coming from the parser
-- | `withStats`         -> to show basic statistic in a formatted way
-- | `withHiddenFiles`   -> to show hidden files under a directory, normally prefixed with `.`
-- | `withTrailingSlash` -> to show trailing slashes for directories
type LsOptions = {
  withStats :: Boolean,
  withHiddenFiles :: Boolean,
  withTrailingSlash :: Boolean
}

-- | Some helper functions to deal with file paths.
-- |
-- | Ideally there should be some kind of
-- | `newtype` just to differentiate between Relative, Absoulte, and Actual Name of a filepath.
-- | But that requires extra work and not sure if it's worth the effort

prefixWith :: FilePath -> FilePath -> FilePath
prefixWith prefix fp = prefix <> "/" <> fp

toActualName :: FilePath -> FilePath
toActualName fp = split (Pattern "/") fp
  # last
  # (\s -> unsafePartial $ cast s)
  where
    cast :: Partial => Maybe FilePath -> FilePath
    cast = fromJust

-- | A file system type is hidden when it's prefixed by `.`
data Visibility = Hidden | Visible

-- | Identify visibility of a given filePath. Being prefixed with a `.` is
-- | considered hidden
-- |
-- | ```purescript
-- | > identifyVisibility "src"
-- | = Visible
-- |
-- | > identifyVisibility ".git"
-- | = Hidden
-- | ```
identifyVisibility :: FilePath -> Visibility
identifyVisibility filePath
  | startsWith (Pattern ".") (toActualName filePath) = Hidden
  | otherwise = Visible

-- | A type to identify whether a particular filepath is a file or directory
data FileSystemType
  = File FilePath Visibility | Directory FilePath Visibility

instance showFileSystemType :: Show FileSystemType where
  show (File f _) = f
  show (Directory d _) = d

isFile :: FileSystemType -> Boolean
isFile (File _ _) = true
isFile _ = false

isDirectory :: FileSystemType -> Boolean
isDirectory = not isFile

isHidden :: FileSystemType -> Boolean
isHidden (File _ Hidden) = true
isHidden (Directory _ Hidden) = true
isHidden _ = false

-- | Then define some behaviour to convert arbitrary filepaths to our data type
toFileSystemType :: FilePath -> NodeFs.Stats -> FileSystemType
toFileSystemType fp s
  | NodeFs.isDirectory s = Directory (toActualName fp) (identifyVisibility fp)
  | otherwise = File (toActualName fp) (identifyVisibility fp)

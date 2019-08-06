module PureShell.Ls.Ls
  ( LsError
  , ls
  ) where

import Prelude

import Control.Monad.Except (ExceptT, withExceptT)
import Data.Array (intercalate)
import Data.List (List, filter, singleton)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Node.FS.Stats (Stats(..))
import Node.Path (FilePath)
import PureShell.Common.MonadFS (class MonadFS, getMetadata, readDir)
import PureShell.Ls.Types (FileSystemType, LsOptions, isDirectory, isFile, isHidden, prefixWith, toFileSystemType)

data LsError = FileOrDirNotExists

instance showLsError :: Show LsError where
  show _ = "No such file or directory"

type FileStats = (FileSystemType /\ Stats)

-- |
-- | The main program of `ls` command
-- |
ls :: ∀ m e. MonadFS e m => FilePath -> LsOptions -> ExceptT LsError m String
ls filePath options = do
  fileStats@(f /\ _) <- getFileStats filePath
  stats <- if isFile f
    then pure $ singleton fileStats -- lift to a List
    else getDirStats filePath options
  pure $ summarizeStats stats
  where
    summarizeStats :: List FileStats -> String
    summarizeStats = map (flip formatStats options) >>> intercalate "\n"

-- |
-- | Read raw metadata then transform it to `FileStats` type
-- |
getFileStats :: ∀ m e. MonadFS e m => FilePath -> ExceptT LsError m FileStats
getFileStats filePath = do
  stats <- withExceptT (const FileOrDirNotExists) (getMetadata filePath)
  pure $ (toFileSystemType filePath stats) /\ stats

-- |
-- | When the given input is a directory, list that directory with the stats
-- |
getDirStats :: ∀ m e
  .  MonadFS e m
  => FilePath
  -> LsOptions
  -> ExceptT LsError m (List FileStats)
getDirStats filePath options = do
  dirContent <- withExceptT (const FileOrDirNotExists) (readDir filePath)
  fileStats <- traverse (prefixWith filePath >>> getFileStats) dirContent
  pure $ respectOptions fileStats
  where
    respectOptions = filter (\(fp /\ _) -> options.withHiddenFiles || not (isHidden fp))

-- | Helper function to format metadata
-- |
-- | TODO: Format date properly
-- | TODO: Give a colour to `Directory`. Might introduce some other monad
formatStats :: FileStats -> LsOptions -> String
formatStats (fileSysType /\ (Stats s)) options = withStats <> formattedPath
  where
    withStats :: String
    withStats = if not options.withStats
      then ""
      else show s.mode <> " " <>
        show s.nlink <> " " <>
        show s.uid <> " " <>
        show s.gid <> " " <>
        show s.size <> " " <>
        show s.mtime <> " "
    formattedPath :: String
    formattedPath = if options.withTrailingSlash && isDirectory fileSysType
      then show fileSysType # (_ <> "/")
      else show fileSysType

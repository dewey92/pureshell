module PureShell.Ls.LsM
  ( LsOptions
  , lsM
  ) where

import Prelude

import Data.Array (foldr)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), fromRight)
import Data.Function.Uncurried (runFn0)
import Data.List (List, filter, singleton)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Node.FS.Stats (Stats(..))
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import PureShell.Common.FileM (class MonadFs, FileSystemType, getMetadata, isDirectory, isHidden, readDir, toFileSystemType, toRelativeOrAbsPath, try)

data LsError = FileOrDirNotExists

instance showLsError :: Show LsError where
  show _ = "No such file or directory"

-- | These options are actually coming from the parser
-- | `withStats`         -> to show basic statistic in a formatted way
-- | `withHiddenFiles`   -> to show hidden files under a directory, normally prefixed with `.`
-- | `withTrailingSlash` -> to show trailing slashes for directories
type LsOptions = {
  withStats :: Boolean,
  withHiddenFiles :: Boolean,
  withTrailingSlash :: Boolean
}

type FileStats = (FileSystemType /\ Stats)
type ErrorOrFileStats = (Either LsError FileStats)

-- | pure version of `ls`
lsM :: forall m e.
  MonadFs e m =>
  FilePath -> LsOptions -> m String
lsM filePath options = safeGetMetadata filePath >>= case _ of
  Left e -> pure $ show e
  Right fileStats@(_ /\ Stats s) -> do
    stats <- if runFn0 s.isFile
      then fileStats # singleton # pure -- lift to a List
      else getDirStats filePath options
    pure $ foldr (\curr acc -> acc <> (formatStats curr options) <> "\n") mempty stats

safeGetMetadata :: forall m e.
  MonadFs e m =>
  FilePath -> m ErrorOrFileStats
safeGetMetadata filePath = do
  stats <- try $ getMetadata filePath
  pure $ bimap (const FileOrDirNotExists) (\s -> (toFileSystemType filePath s) /\ s) stats

-- | When the given input is a directory, list that directory with the stats
-- | then join the result with a breakline
getDirStats :: forall m e.
  MonadFs e m =>
  FilePath -> LsOptions -> m (List FileStats)
getDirStats filePath options = listDirsInside filePath >>= concludeStats >>> pure
  where
    concludeStats :: List ErrorOrFileStats -> List FileStats
    concludeStats = filter (case _ of
        Left _ -> false
        Right (fp /\ s) -> options.withHiddenFiles || not (isHidden fp)
      )
      >>> (\s -> unsafePartial $ eliminateLeft s)
    eliminateLeft :: Partial => List ErrorOrFileStats -> List FileStats
    eliminateLeft = map (fromRight)

-- | Read all files and directories in a given filepath
listDirsInside :: forall m e.
  MonadFs e m =>
  FilePath -> m (List ErrorOrFileStats)
listDirsInside dir = readDir dir >>= traverse (appendDirPrefix >>> safeGetMetadata)
  where
    appendDirPrefix = toRelativeOrAbsPath dir

-- | Helper function to format the thing
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

module PureShell.Ls.LsM ( lsM ) where

import Prelude

import Data.Array (intercalate)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), fromRight)
import Data.List (List, filter, singleton)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Node.FS.Stats (Stats(..))
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import PureShell.Common.FileM (class MonadFs, getMetadata, readDir, try)
import PureShell.Ls.Types (FileSystemType, LsOptions, isDirectory, isFile, isHidden, prefixWith, toFileSystemType)

data LsError = FileOrDirNotExists

instance showLsError :: Show LsError where
  show _ = "No such file or directory"

type FileStats = (FileSystemType /\ Stats)
type ErrorOrFileStats = (Either LsError FileStats)

-- | The main program of `ls` command
lsM :: forall m e. MonadFs e m => FilePath -> LsOptions -> m String
lsM filePath options = safeGetMetadata filePath >>= case _ of
  Left e -> pure $ show e
  Right fileStats@(f /\ _) -> do
    stats <- if isFile f
      then fileStats # singleton # pure -- lift to a List
      else getDirStats filePath options
    pure $ summarizeStats stats
    where
      summarizeStats :: List FileStats -> String
      summarizeStats = map (flip formatStats options) >>> intercalate "\n"

-- | Safely get metadata (stats) and transforms to `FileStats` if succeeds
safeGetMetadata :: forall m e. MonadFs e m => FilePath -> m ErrorOrFileStats
safeGetMetadata filePath = do
  stats <- try $ getMetadata filePath
  pure $ bimap
    (const FileOrDirNotExists)
    (\s -> (toFileSystemType filePath s) /\ s)
    stats

-- | When the given input is a directory, list that directory with the stats
-- | then join the result with a breakline
getDirStats :: forall m e. MonadFs e m => FilePath -> LsOptions -> m (List FileStats)
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
listDirsInside :: forall m e. MonadFs e m => FilePath -> m (List ErrorOrFileStats)
listDirsInside dir = readDir dir >>= traverse (prefixWith dir >>> safeGetMetadata)

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

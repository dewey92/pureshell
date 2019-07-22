module PureShell.Ls.LsM
  ( LsOptions
  , lsM
  ) where

import Prelude

import Data.Array (foldr, last)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), fromRight)
import Data.Function.Uncurried (runFn0)
import Data.List (List, filter, singleton)
import Data.Maybe (maybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Node.FS.Stats (Stats(..))
import Node.Path (FilePath)
import Options.Applicative.Internal.Utils (startsWith)
import Partial.Unsafe (unsafePartial)
import PureShell.Common.FileM (class MonadFs, getStat, readDir, try)

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

type FileStats = (FilePath /\ Stats)
type ErrorOrFileStats = (Either LsError FileStats)

-- | pure version of `ls`
lsM :: forall m e.
  MonadFs e m =>
  FilePath -> LsOptions -> m String
lsM filePath options = getStatSafe filePath >>= case _ of
  Left e -> pure $ show e
  Right fileStats@(_ /\ Stats s) -> do
    stats <- if runFn0 s.isFile
      then pure $ singleton fileStats -- lift to a List
      else getDirStats filePath options
    pure $ foldr (\curr acc -> acc <> (formatStats curr options) <> "\n") mempty stats

getStatSafe :: forall m e.
  MonadFs e m =>
  FilePath -> m ErrorOrFileStats
getStatSafe filePath = do
  stats <- try $ getStat filePath
  pure $ bimap (const FileOrDirNotExists) (filePath /\ _) stats

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
        Right (fp /\ (Stats s)) ->
          -- | Exclude hidden files (prefixed with `.`) when `options.withHiddelFiles` is false
          options.withHiddenFiles || not (startsWith (Pattern ".") $ removePath fp)
      )
      >>> (\s -> unsafePartial $ eliminateLeft s) -- FIXME: not sure why point-free doesn't work
    eliminateLeft :: Partial => List ErrorOrFileStats -> List FileStats
    eliminateLeft = map (fromRight)

-- | Read all files and directories in a given filepath
listDirsInside :: forall m e.
  MonadFs e m =>
  FilePath -> m (List ErrorOrFileStats)
listDirsInside dir = readDir dir >>= traverse (appendDirPrefix >>> getStatSafe)
  where
    appendDirPrefix content = dir <> "/" <> content

-- | Helper function to format the thing
formatStats :: FileStats -> LsOptions -> String
formatStats (filePath /\ (Stats stats)) options = withStats <> formattedPath
  where
    withStats :: String
    withStats = if not options.withStats
      then ""
      else show stats.mode <> " " <>
        show stats.nlink <> " " <>
        show stats.uid <> " " <>
        show stats.gid <> " " <>
        show stats.size <> " " <>
        show stats.mtime <> " "
    formattedPath :: String
    formattedPath = if options.withTrailingSlash && not (runFn0 stats.isFile)
      then removePath filePath # (_ <> "/")
      else removePath filePath

removePath :: FilePath -> FilePath
removePath fp = split (Pattern "/") fp
  # last
  # maybe fp identity
